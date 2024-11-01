(ns space-game.llm
  (:require [clj-http.client :as http]
            [cheshire.core :as json]
            [clojure.string :as str]))

;; Configuration for Ollama
(def llm-config
  {:base-url "http://localhost:11434/api"
   :model "mistral"
   :temperature 0.7})

;; Enhanced prompt templates with better naming guidance
(def prompt-templates
  {:system
   "Generate a rich description of a star system in a space exploration game.
    System Name: %s
    Coordinates: %s
    
    Rules:
    1. Each planet should have a unique, creative name
    2. Include varied planet types (terrestrial, gas giants, ice worlds, etc.)
    3. Add interesting resources and phenomena
    4. Include potential dangers or mysteries
    
    Respond ONLY with JSON in exactly this format:
    {
      \"name\": \"string\",
      \"stars\": [
        {
          \"type\": \"string\",
          \"color\": \"string\"
        }
      ],
      \"planets\": [
        {
          \"name\": \"string\",
          \"type\": \"string\",
          \"description\": \"string\",
          \"resources\": [\"string\"]
        }
      ],
      \"features\": [\"string\"],
      \"dangers\": [\"string\"]
    }"})

;; Ollama API integration (unchanged)
(defn generate-ollama-response
  "Makes a call to Ollama API"
  [prompt]
  (try
    (let [response (http/post (str (:base-url llm-config) "/generate")
                             {:content-type :json
                              :body (json/generate-string
                                     {:model (:model llm-config)
                                      :prompt prompt
                                      :system "You are a creative sci-fi writer specializing in generating unique and interesting space content."
                                      :temperature (:temperature llm-config)})
                              :throw-exceptions false})]
      (if (= (:status response) 200)
        (-> response :body (json/parse-string true) :response)
        (throw (Exception. (str "API error: " (:status response))))))
    (catch Exception e
      (println "\nError calling Ollama:" (.getMessage e))
      (println "Make sure Ollama is installed and running!")
      nil)))

;; Helper function to extract JSON from LLM response (unchanged)
(defn extract-json
  "Attempts to extract valid JSON from LLM response"
  [response]
  (try
    (let [json-str (-> response
                       (str/replace #"```json" "")
                       (str/replace #"```" "")
                       str/trim)]
      (json/parse-string json-str true))
    (catch Exception e
      nil)))

;; Planet name generation helper
(defn generate-planet-name
  "Generate a unique name for a planet based on its characteristics"
  [system-name planet-type]
  (let [prompt (str "Generate a single unique and interesting name for a " planet-type 
                    " planet in the " system-name " system. The name should reflect "
                    "the planet's characteristics. Respond with only the name, no explanation.")
        response (generate-ollama-response prompt)]
    (-> response
        str/trim
        (str/replace #"[^\w\s-]" "")))) ; Clean up any special characters

(defn generate-system-description
  "Generate a detailed star system description using Ollama"
  [system-name coordinates]
  (let [prompt (format (:system prompt-templates)
                      system-name
                      (str coordinates))
        response (generate-ollama-response prompt)]
    (if-let [parsed (extract-json response)]
      parsed
      ;; More interesting fallback response
      {:name (or system-name "Unknown System")
       :stars [{:type "Main Sequence"
               :color (rand-nth ["Yellow" "Orange" "Red" "Blue" "White"])}]
       :planets [(let [type (rand-nth ["Rocky" "Gas Giant" "Ice World" "Desert" "Ocean"])]
                  {:name (str (rand-nth ["Nova" "Alpha" "Beta" "Gamma"]) "-"
                             (rand-int 1000))
                   :type type
                   :description (str "A mysterious " (str/lower-case type) " world")
                   :resources [(rand-nth ["Minerals" "Water Ice" "Rare Gases" 
                                        "Precious Metals" "Energy Crystals"])]})
                ]
       :features ["Unusual energy readings" "Asteroid field"]
       :dangers ["Unknown anomalies" "Solar radiation"]})))

;; Test function
(defn test-planet-names []
  (println "\nTesting planet name generation...")
  (println "\nGenerating names for different planet types:")
  
  (doseq [planet-type ["desert" "ice" "ocean" "jungle" "volcanic"]]
    (let [name (generate-planet-name "Test System" planet-type)]
      (println (str planet-type ": " name))))
  
  (println "\nGenerating full system description...")
  (let [system (generate-system-description "Nova Centauri" [2 3 1])]
    (println "\nSystem planets:")
    (doseq [planet (:planets system)]
      (println (format "- %s (%s): %s"
                      (:name planet)
                      (:type planet)
                      (:description planet))))))
