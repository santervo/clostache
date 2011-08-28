(ns clostache.parser
  "A parser for mustache templates."
  (:use [clojure.contrib.string :only (map-str)]))

(defrecord Section [name body start end inverted])

(defn- replace-all
  "Applies all replacements from the replacement list to the string."
  [string replacements]
  (reduce (fn [string [from to]]
            (.replaceAll string from to)) string replacements))

(defn- escape-html
  "Replaces angle brackets with the respective HTML entities."
  [string]
  (replace-all string [["&" "&amp;"]
                       ["\"" "&quot;"]
                       ["<" "&lt;"]
                       [">" "&gt;"]]))

(defn- create-variable-replacements
  "Creates pairs of variable replacements from the data."
  [data]
  (apply concat
         (for [k (keys data)]
           (let [var-name (name k)
                 var-value (k data)]
             (if (instance? String var-value)
               [[(str "\\{\\{\\{\\s*" var-name "\\s*\\}\\}\\}") var-value]
                [(str "\\{\\{\\&s*" var-name "\\s*\\}\\}") var-value]
                [(str "\\{\\{\\s*" var-name "\\s*\\}\\}")
                 (escape-html var-value)]])))))

(defn- remove-comments
  "Removes comments from the template."
  [template]
  (replace-all template [["\\{\\{\\![^\\}]*\\}\\}" ""]]))

(defn- section-start [template]
  (first (sort (filter #(not (neg? %)) (map #(.indexOf template %) ["{{#" "{{^"])))))

(defn- extract-section
  "Extracts the outer section from the template."
  [template]
  (let [start (section-start template)]
    (when start
      (let [body-start (+ (.indexOf template "}}" start) 2)
            section-name (.trim (subs template (+ start 3) (- body-start 2)))
            end-tag-pattern (re-pattern (str "\\{\\{\\/\\s*" section-name "\\s*\\}\\}"))
            end-tag-matcher (re-matcher end-tag-pattern template)
            body-end (and (.find end-tag-matcher body-start) (.start end-tag-matcher))
            end (+ body-end (.length (.group end-tag-matcher)))
            body (subs template body-start body-end)
            start-token (subs template start (+ start 3))
            inverted (= start-token "{{^")]
        (Section. section-name body start end inverted)))))

(defn render
  "Renders the template with the data."
  [template data]
  (let [replacements (create-variable-replacements data)
        template (remove-comments template)
        section (extract-section template)]
    (if (nil? section)
      (replace-all template replacements)
      (let [before (.substring template 0 (:start section))
            after (.substring template (:end section))
            section-data ((keyword (:name section)) data)]
        (recur
         (str before
              (if (:inverted section)
                (if (or (and (vector? section-data) (empty? section-data))
                        (not section-data))
                  (:body section))
                (if section-data
                  (let [section-data (if (or (sequential? section-data) (map? section-data)) section-data {})
                        section-data (if (sequential? section-data) section-data [section-data])]
                    (map-str (fn [m] (render (:body section) m)) section-data))))
              after) data)))))
