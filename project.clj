(defproject lein-swagger "0.1.0"
  :description "A leiningen plugin to generate swagger clients in clojure"
  :url "http://github.com/rbuchmann/lein-swagger"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[io.swagger/swagger-codegen "2.2.1"]
                 [org.clojure/data.json "0.2.6"]
                 [camel-snake-kebab "0.4.0"]]
  :eval-in-leiningen true)
