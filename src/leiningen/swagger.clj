(ns leiningen.swagger
  (:import (io.swagger.codegen CliOption
                               ClientOptInput
                               CodegenConfig
                               DefaultGenerator)
           io.swagger.codegen.config.CodegenConfigurator)
  (:require [leiningen.core.main    :as lein]
            [clojure.edn            :as edn]
            [clojure.data.json      :as json]
            [camel-snake-kebab.core :refer [->kebab-case-symbol
                                            ->kebab-case-keyword]]
            [clojure.pprint         :refer [code-dispatch with-pprint-dispatch pprint]]))

(defn check-keys [m ks]
  (when-let [k (some #(when-not (m %)
                        %)
                     ks)]
    (lein/abort "Missing required key:" k)))

(defn gen-swagger [opts]
  (check-keys opts [:input :path :name])
  (let [{:keys [input path name]} opts
        config-input (.toClientOptInput (doto (CodegenConfigurator.)
                                          (.setLang "clojure")
                                          (.setInputSpec input)
                                          (.setOutputDir path)))
        additional-options (doto (-> config-input
                                     (.getConfig)
                                     (.additionalProperties))
                             (.put "projectName" name))
        gen (-> (DefaultGenerator.)
                (.opts config-input))]
    (try (.generate gen)
         (catch Exception e
           (lein/abort "An error occurred: " e)))))

(defn parse-args [args]
  (->> args
       (partition 2)
       (map (fn [[k v]] [(edn/read-string k) v]))
       (into {})))

(defn kfn [s]
  (if (.startsWith s "/")
    s
    (keyword s)))

(def tj (json/read-str (slurp "evaluate.article.json") :key-fn kfn))

(def tp (first (:paths tj)))

(defn method->symbol [method]
  (symbol "http" (name method)))

(def symbolize-name (comp ->kebab-case-symbol :name))
(def keywordize-name (comp ->kebab-case-keyword :name))

(defn param-vector [parameters]
  (let [{:keys [required optional]} (group-by #(if (:required %)
                                                 :required
                                                 :optional)
                                              parameters)
        optionals-map {:keys (mapv symbolize-name optional)}]
    (-> (mapv symbolize-name required)
        (conj '& optionals-map))))

(defmulti gen-param-type first)

(defmethod gen-param-type "query" [[_ params]]
  {:query-params
   (into {} (map (juxt :name symbolize-name) params))})

(defmethod gen-param-type "header" [[_ params]]
  {:headers
   (into {} (map (juxt :name symbolize-name) params))})

(defn gen-parameters [parameters]
  (let [params-by-type (group-by :in parameters)]
    (map gen-param-type params-by-type)))

(defn gen-method [path method info]
  (let [{:keys [parameters summary operationId produces]} info
        method-name (->kebab-case-symbol operationId)]
    (list 'defn method-name summary (param-vector parameters)
       ((method->symbol method) path
        (apply merge (gen-parameters parameters))))))

(defn gen-path [path desc]
  (for [[method info] desc]
    (gen-method path method info)))

(defn format-method [form]
  (with-pprint-dispatch code-dispatch (pprint form)))

(defn gen-custom [src]
  (doseq [s (map format-method (apply gen-path src))]
    (println s)))

(defn swagger
  "Generates a swagger client from the specifications. Mandatory arguments:\n:name -> client project name\n:input -> swagger input file location\n:path -> output path"
  [project & args]
  (gen-swagger (parse-args args)))
