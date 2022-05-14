(ns joyride.sci
  (:require ["fs" :as fs]
            ["path" :as path]
            ["vscode" :as vscode]
            [clojure.string :as str]
            [joyride.db :as db]
            [joyride.config :refer [workspace-scripts-path] :as config]
            [joyride.utils :as utils]
            [sci-configs.funcool.promesa :as pconfig]
            [sci.core :as sci]))

(sci/alter-var-root sci/print-fn (constantly *print-fn*))

(def joyride-ns (sci/create-ns 'joyride.core nil))

(defn ns->path [namespace]
  (-> (str namespace)
      (munge)
      (str/replace  "." "/")
      (str ".cljs")))

(defn source-script-ns [namespace {:keys [from]}]
  (let [search-dirs (case from
                      user [(config/user-abs-scripts-path)]
                      workspace [(config/workspace-abs-scripts-path)]
                      [(config/user-abs-scripts-path) (config/workspace-abs-scripts-path)])
        path-to-ns (ns->path namespace)
        search-paths (map #(path/join % path-to-ns) (filter identity search-dirs))
        matching-files (filter fs/existsSync search-paths)]
    (case (count matching-files)
      1 (str (fs/readFileSync (first matching-files)))
      0 (do
          (utils/say-error (str "Could not find " 
                                (when from (str from " "))
                                "file: " path-to-ns))
          "")
      (do
        (utils/say-error (str "More than one file found: " (set matching-files)
                              ". Consider qualifying with :from."))
        ""))))

(def !ctx
  (volatile!
   (sci/init {:classes {'js goog/global
                        :allow :all}
              :namespaces (assoc
                           (:namespaces pconfig/config)
                           'joyride.core {'*file* sci/file
                                          'extension-context (sci/copy-var db/extension-context joyride-ns)
                                          'invoked-script (sci/copy-var db/invoked-script joyride-ns)
                                          'output-channel (sci/copy-var db/output-channel joyride-ns)})
              :load-fn (fn [{:keys [namespace opts]}]
                         (cond
                           (symbol? namespace)
                           {:source (source-script-ns namespace opts)}
                           (string? namespace) ;; node built-in or npm library
                           (if (= "vscode" namespace)
                             (do (sci/add-class! @!ctx 'vscode vscode)
                                 (sci/add-import! @!ctx (symbol (str @sci/ns)) 'vscode (:as opts))
                                 {:handled true})
                             (let [mod (js/require namespace)
                                   ns-sym (symbol namespace)]
                               (sci/add-class! @!ctx ns-sym mod)
                               (sci/add-import! @!ctx (symbol (str @sci/ns)) ns-sym
                                                (or (:as opts)
                                                    ns-sym))
                               {:handled true}))))})))

(def !last-ns (volatile! @sci/ns))

(defn eval-string [s]
  (sci/binding [sci/ns @!last-ns]
    (let [rdr (sci/reader s)]
      (loop [res nil]
        (let [form (sci/parse-next @!ctx rdr)]
          (if (= :sci.core/eof form)
            (do
              (vreset! !last-ns @sci/ns)
              res)
            (recur (sci/eval-form @!ctx form))))))))
