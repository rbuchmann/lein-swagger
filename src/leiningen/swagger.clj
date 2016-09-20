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
            [clojure.pprint         :refer [code-dispatch with-pprint-dispatch pprint]]
            [clojure.string         :as str]
            [clojure.java.io        :as io]))

(defn check-keys [m ks]
  (when-let [k (some #(when-not (m %)
                        %)
                     ks)]
    (lein/abort "Missing required key:" k)))

(defn parse-args [args]
  (->> args
       (partition 2)
       (map (fn [[k v]] [(edn/read-string k) v]))
       (into {})))

(defn kfn [s]
  (if (.startsWith s "/")
    s
    (keyword s)))

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

(defn patternize-name [param]
  (re-pattern (str "\\{" (:name param) "\\}")))

(defmethod gen-param-type "path" [[_ params]]
  (when (not-empty params)
    {:path-params (map (juxt patternize-name symbolize-name) params)}))

(defn gen-parameters [parameters]
  (let [params-by-type (group-by :in parameters)]
    (map gen-param-type params-by-type)))

(defn gen-param-path [path params]
  (when (not-empty params)
    `(~'-> ~path
      ~@(for [p params]
          (list* 'str/replace p)))))

(defn gen-method [path method info]
  (let [{:keys [parameters summary operationId produces]} info
        method-name (->kebab-case-symbol operationId)
        client-params (apply merge (gen-parameters parameters))
        url (or (gen-param-path path (:path-params client-params)) path)]
    (list 'defn method-name summary (param-vector parameters)
       (list (method->symbol method) url
        (dissoc client-params :path-params)))))

(defn gen-path [path desc]
  (for [[method info] desc]
    (gen-method path method info)))

(defn format-method [form]
  (with-out-str
    (with-pprint-dispatch code-dispatch (pprint form))))

(defn gen-header [name]
  (format-method
   (list 'ns (symbol (str "swagger-clients." name))
         '(:require [clj-http.client :as http]))))

(defn filenamify [s]
  (str "src/swagger-clients/"
       (str/replace s #"-" "_")
       ".clj"))

(defn gen-source [src]
  (str/join "\n" (->> src
                      :paths
                      (map #(apply gen-path %))
                      (apply concat)
                      (map format-method))))

(defn gen-swagger [src name]
  (let [json (json/read-str (slurp src) :key-fn kfn)
        source (gen-source json)
        target (filenamify name)]
    (io/make-parents target)
    (spit target (str (gen-header name)
               "\n"
               source))))
(defn swagger
  "Generates a swagger client from the specifications. Mandatory arguments:\n:name -> client project name\n:src -> swagger input file location"
  [project & args]
  (let [{:keys [src name] :as m} (parse-args args)]
    (check-keys m [:src :name])
    (gen-swagger src name)))
