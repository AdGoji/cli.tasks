(ns adgoji.cli.tasks
  (:require [spartan.spec :as s]
            [clojure.edn]))

;; export BABASHKA_CLASSPATH="$(clojure -Sdeps '{:deps {spartan.spec {:git/url "https://github.com/borkdude/spartan.spec" :sha "104129aae9eab6dd4622937d0f46abeed9c9c537"}}}' -Spath)"

(defn upper-cased? [x]
  (let [xs (str x)]
    (= xs (clojure.string/upper-case xs))))

(defn upper-cased-symbol? [x]
  (and (symbol? x)
       (upper-cased? x)))

(def opt-types
  '#{bool char code edn file float int kw regexp sym str})

(defn strlen-1? [x]
  (= 1 (count (str x))))

(defn strlen-2+? [x]
  (< 1 (count (str x))))

(comment

  (require '[spartan.spec :as s]))

(s/def ::short-option (s/or ::nil #{nil '_} ;; FIXME :nil is not accepted by https://github.com/borkdude/edamame/issues/40
                            :v (s/and symbol? strlen-1?)) )

(s/def ::long-option (s/or ;::nil #{nil '_} ;; FIXME :nil is not accepted by https://github.com/borkdude/edamame/issues/40
                      :v (s/and symbol? strlen-2+?)) )

(s/def ::option (s/cat :short-opt ::short-option
                       :long-opt ::long-option
                       :argname (s/? upper-cased-symbol?)
                       :default (s/? (complement upper-cased-symbol?)) ;; DSL extension!
                       :opt-type opt-types
                       :desc string?))

(s/def ::tasks (s/cat
                                        ; :description (s/? string?)
                :options (s/* ::option)
                ))

(defn conform-tasks [forms]
  (let [nil-or-value (fn [x]
                       (let [[t v] x]
                         (case t
                           :v v
                           ::nil nil)))]
    (update (s/conform ::tasks forms)
            :options (fn [options]
                       (map (fn [option]
                              (-> option
                                  (update :short-opt nil-or-value)
                                  (update :long-opt nil-or-value)))
                            options)))))



;; TODO better error messages etc
(defn opt->cli-opt [{:keys [opt-type short-opt long-opt argname desc opts] :as opt}]
  (let [base
        [(some->> short-opt (str "-"))
         (some->> (cond-> long-opt
                    (and long-opt argname)
                    (str " " argname))
                  (str "--"))
         desc]
        kw-opts
        (case opt-type
          bool (if-not argname
                 {:parse-fn (comp boolean identity)}
                 {:parse-fn #(case %
                               "true" true
                               "false" false
                               %)
                  :validate [#(contains? #{true false} %) "Must be a boolean"]})
          char {:parse-fn first :validate [char?]}
          edn {:parse-fn (fn [s]
                           (let [edn-str (if (= (first s) \@)
                                           (slurp (clojure.java.io/file (subs s 1)))
                                           s)]
                             (clojure.edn/read-string edn-str)))}

          (code file) (throw (ex-info "not supported" {:opt opt-type}))

          float {:parse-fn #(Double/parseDouble %)
                 :validate [float? "Must be a floating point number"]}
          int (if-not argname
                {:default 0 :update-fn inc}
                {:parse-fn #(Integer/parseInt %) ;; TODO add Long/parseLong to Babashka
                 :validate [integer? "Must be an integer"]})
          kw {:parse-fn keyword
              :validate [keyword? "Must be a keyword"]}
          (regexp ) (throw (ex-info "not supported" {:opt opt-type}))
          sym {:parse-fn symbol
               :validate [symbol? "Must be a symbol"]}
          nil)]
    (into base (mapcat identity (merge kw-opts opts)))))

(def tasks (atom {}))

(defn parse-options [forms]
  (let [{:keys [description options]} (conform-tasks forms)]
    (map opt->cli-opt options)))


(defn list-tasks []
  (clojure.string/join "\n" (map (fn [[task {:keys [description]}]]
                                     (str "* " task " : " description)) (sort-by key @tasks))))

(defn exit
  ([status]
   (exit status nil))
  ([status msg]
   (throw (ex-info "" {:type :exit
                       :exit/code status
                       :exit/message msg}))))


(defn run-tasks [args]
  (let [[dispatch & task-args] args]
    (if-let [{f :fn} (and dispatch (get @tasks (symbol dispatch)))]
      (f task-args)
      (do
        (println (str "No task found for '" dispatch "'"))
        (println)
        (if (seq @tasks)
          (do
            (println "Existing tasks")
            (println (list-tasks)))
          (println "No existing tasks"))
        (exit 1)))))

(require '[clojure.tools.cli :as cli])

;; Quick hack

(defn quick-parse-options [opts]
  (loop [acc []
         opt []
         [current & [next & left :as cleft] :as queue] opts]
    (if (empty? queue)
      (if (empty? opt)
        acc
        (throw (ex-info (str "Not fully parsed " (pr-str opt)) {:left opt :acc acc})))
      (if (and (opt-types current)
               (string? next))
        (let [[part left] (split-with #(keyword? (first %)) (partition-all 2 left))
              rst (mapcat identity left)]
          (recur (conj acc (conj opt current next (into {} (map vec) part)))
                 []
                 rst))
        (recur acc
               (conj opt current)
               cleft)))))

(defn _->nil [x]
  (if (= '_ x)
    nil
    x))

(defn annotate-option [option]
  (let [[opts desc opt-type & rst] (reverse option)
        [short-opt long-opt & rst0] (reverse rst)
        [argname default]
        (if (upper-cased-symbol? (first rst0))
          [(first rst0) (second rst0)]
          [nil (first rst0)])]
    {:opts (merge {:default default} opts) :desc desc :opt-type opt-type
     :short-opt (_->nil short-opt) :long-opt (_->nil long-opt)
     :id long-opt
     :argname argname}))

(defn quick-parse-options0 [options]
  (->> (quick-parse-options options)
       (map annotate-option)
       (map opt->cli-opt)))

(def ^:dynamic *opts* {})
(def ^:dynamic *args* {})

(defn register-task [task-sym {f :fn :keys [description]} config]
  (swap! tasks update task-sym
         (fn [prev]
           (when prev
             (println (format "WARN: deftask %s/%s was overridden\n" *ns* task-sym)))

           {:fn (fn [cli-args]
                  (let [cli-options #_(parse-options config)
                        (quick-parse-options0 config)
                        {:keys [errors options] :as parsed-args}
                        (cli/parse-opts cli-args cli-options)]
                    (if-let [errors (seq errors)]
                      (do
                        (println "Invalid")
                        (println errors)
                        (exit 1))
                      (binding [*opts* options
                                *args* (into [task-sym] cli-args)]
                        (f options)))))
            :description description})))

(import '[java.lang ProcessBuilder$Redirect])
(require
 '[clojure.string :as str]
 '[clojure.java.io :as io])

(defn shell-command
  "Executes shell command.
  Accepts the following options:
  `:input`: instead of reading from stdin, read from this string.
  `:to-string?`: instead of writing to stdoud, write to a string and
  return it.
  `:throw?`: Unless `false`, exits script when the shell-command has a
  non-zero exit code, unless `throw?` is set to false."
  ([args] (shell-command args nil))
  ([args {:keys [:input :to-string? :throw? :show-errors? :timeout ]
          :or {throw? true
               show-errors? true}}]
   (let [args (mapv str args)
         pb (cond-> (ProcessBuilder. ^java.util.List args)
              show-errors? (.redirectError ProcessBuilder$Redirect/INHERIT)
              (not to-string?) (.redirectOutput ProcessBuilder$Redirect/INHERIT)
              (not input) (.redirectInput ProcessBuilder$Redirect/INHERIT))
         proc (.start pb)]
     (when input
       (with-open [w (io/writer (.getOutputStream proc))]
         (binding [*out* w]
           (print input)
           (flush))))
     (let [string-out
           (when to-string?
             (let [sw (java.io.StringWriter.)]
               (with-open [w (io/reader (.getInputStream proc))]
                 (io/copy w sw))
               (str sw)))
           prom (promise)
           _ (future
               (let [exit-code (.waitFor proc)]
                 (when (and throw? (not (zero? exit-code)))
                   (System/exit exit-code))
                 (deliver prom {:out string-out
                                :exit exit-code})))]
       (if timeout
         (let [v (deref prom timeout ::timeout)]
           (if (= v ::timeout)
             (exit 1 (str "Timed out on " (clojure.string/join ", " args)))
             v))
         @prom)))))

#?(:bb
   ;; TODO handle interrupt
   (defn handle-in-clj [msg cli-args]
     (when msg
       (println msg))
     (let [cmd (into ["clj" *file*] (map str) cli-args)]
       (shell-command cmd))))

(defmacro run-in-bb [& body]
  #?(:bb `(do ~@body)))

(defmacro delegate-to-clj [& body]
  #?(:bb `(handle-in-clj "Delegating to clj" *args*)
     :clj `(do ~@body)))

(comment
  (->>
   (quick-parse-options
    '[a a-option AA kw    "The option." :a 1 :b 2
      c counter       int   "The counter."
      e entry     sym   "An entrypoint symbol."
      f flag          bool  "Enable flag."
      o o-option   str   "The other option."
      z nil  str   "The other option."
      nil yy   str   "The other option."])
   (map annotate-option)
   (map opt->cli-opt))

  )

(defmacro deftask
  "Define a boot task."
  [sym desc opts & fn-body]
  (let [opt-syms (keep second (quick-parse-options opts))]
    `(let [f# (fn [{:keys [~@opt-syms]}]
                (do ~@fn-body))]
       (register-task (quote ~sym)
                      {:fn f# :description ~desc}
                      (quote ~opts)))))


  ;; https://github.com/boot-clj/boot/wiki/Task-Options-DSL
(deftask demo
  "Demonstrate the task options DSL."
  [a a-option VAL  200 kw    "The option."
   c counter       int   "The counter."
   e entry    VAL  sym   "An entrypoint symbol."
   f flag          bool  "Enable flag."
   o o-option VAL  str   "The other option."
   z zz VAL  str   "The other option."
   nil yy VAL  str   "The other option."]
  (prn *opts*))

(defn run-tasks-with-exit-handler [args f]
  (try
    (run-tasks args)
    (f {:exit/code 0})

    (catch Exception ex
      (let [{ex-type :type :keys [:exit/code :exit/message] :as data} (ex-data ex)]
        (if (= ex-type :exit)
          (f data)
          (f {:exit/code code
              :exit/message message
              :exception ex}))))))

(defn run-main [args]
  (run-tasks-with-exit-handler args
                               (fn [{ex-type :type :keys [:exit/code :exit/message exception] :as data}]
                                 (if message
                                   (println message)
                                   (when exception
                                     (println "Uncaught Exception")
                                     (.printStackTrace ^Throwable exception)))
                                 (shutdown-agents)
                                 (System/exit (or code 1)))))

(defn -main [& args]
  (run-main args))


(defn test-main [& args]
  (let [os (java.io.StringWriter.)
        es (java.io.StringWriter.)]
    (binding [*out* os
              *err* es]
      (let [{:keys [exit/message exit/code] :or {code 0}} (run-tasks-with-exit-handler args identity)]
        (flush)
        (cond-> {}
          (not (clojure.string/blank? (str os))) (assoc :out (str os))

          (not (clojure.string/blank? (str es))) (assoc :err (str es))

          true
          (->
           (assoc :exit code)
           (update (if (and code (zero? code)) :out :err) str message)))))))


#?(:bb (apply -main *command-line-args*))
