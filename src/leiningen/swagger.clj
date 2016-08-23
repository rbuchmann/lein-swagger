(ns leiningen.swagger
  (:import (io.swagger.codegen CliOption
                               ClientOptInput
                               CodegenConfig
                               DefaultGenerator)
           io.swagger.codegen.config.CodegenConfigurator)
  (:require [leiningen.core.main :as lein]
            [clojure.edn         :as edn]))

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

(defn swagger
  "Generates a swagger client from the specifications. Mandatory arguments:\n:name -> client project name\n:input -> swagger input file location\n:path -> output path"
  [project & args]
  (gen-swagger (parse-args args)))
