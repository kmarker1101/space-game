(ns space-game.core
  (:require [clojure.string :as str]
            [cheshire.core :as json]
            [space-game.llm :as llm]))

;; Enhanced Data Structures
(def initial-game-state
  {:player {:name ""
           :location {:system "Sol" :coordinates [0 0 0]}
           :inventory []
           :health 100
           :fuel 1000}
   :discovered-systems #{}
   :current-system {}
   :current-planet nil
   :message-log []})

;; Helper Functions
(defn log-message
  "Add a message to the game's message log"
  [state message]
  (update state :message-log conj message))

(defn display-system-info
  "Format system information for display"
  [system]
  (when system
    (str "\nSystem: " (:name system)
         "\nStars: " (count (:stars system))
         "\nPlanets: " (count (:planets system))
         "\nNotable Features: " (str/join ", " (:features system))
         "\nPotential Dangers: " (str/join ", " (:dangers system)))))

(defn display-planet-info
  "Format planet information for display"
  [planet]
  (when planet
    (str "\nPlanet: " (:name planet)
         "\nTerrain: " (:terrain planet)
         "\nAtmosphere: " (:atmosphere planet)
         "\nNotable Locations: " (str/join ", " (:locations planet))
         "\nPhenomena: " (str/join ", " (:phenomena planet)))))

;; Enhanced Command System
(defn parse-command [input]
  (let [[command & args] (str/split (str/lower-case input) #"\s+")]
    {:command command
     :args args}))

(defmulti execute-command
  (fn [game-state command-map] (:command command-map)))

(defmethod execute-command "scan"
  [game-state _]
  (let [current-loc (get-in game-state [:player :location])
        _ (println "\nScanning system" (:system current-loc) "...")
        system-data (llm/generate-system-description 
                     (or (:system current-loc) "Unknown System")
                     (:coordinates current-loc))]
    
    ;; Format and display system information
    (println "\n=== System Report ===")
    (println (str "System Name: " (:name system-data)))
    
    ;; Display star information
    (println "\nStars:")
    (doseq [star (:stars system-data)]
      (println (format "- %s (%s)" (:type star) (:color star))))
    
    ;; Display planet information
    (println "\nPlanets:")
    (doseq [planet (:planets system-data)]
      (println (format "- %s" (:name planet)))
      (println (format "  Type: %s" (:type planet)))
      (when (:description planet)
        (println (format "  Description: %s" (:description planet))))
      (when (seq (:resources planet))
        (println (format "  Resources: %s" (str/join ", " (:resources planet))))))
    
    ;; Display features and dangers
    (when (seq (:features system-data))
      (println "\nNotable Features:")
      (doseq [feature (:features system-data)]
        (println (format "- %s" feature))))
    
    (when (seq (:dangers system-data))
      (println "\nPotential Dangers:")
      (doseq [danger (:dangers system-data)]
        (println (format "- %s" danger))))
    
    (-> game-state
        (assoc :current-system system-data)
        (update :discovered-systems conj (:name system-data))
        (log-message "System scan complete."))))

(defmethod execute-command "jump"
  [game-state {:keys [args]}]
  (if (< (get-in game-state [:player :fuel]) 100)
    (log-message game-state "Not enough fuel!")
    (let [new-system (or (first args) "Unknown System")
          new-coords [(rand-int 100) (rand-int 100) (rand-int 100)]]
      (println "\nInitiating jump sequence to" new-system "...")
      (println "Jump complete!")
      (-> game-state
          (update-in [:player :fuel] - 100)
          (assoc-in [:player :location] 
                    {:system new-system :coordinates new-coords})
          (assoc :current-planet nil)
          (log-message (str "Jumped to system " new-system))))))

(defmethod execute-command "explore"
  [game-state {:keys [args]}]
  (if-let [planet-name (first args)]
    (if-let [planet (first (filter #(= (str/lower-case (:name %)) 
                                      (str/lower-case planet-name))
                                  (get-in game-state [:current-system :planets])))]
      (let [planet-data (llm/generate-planet-name 
                         (:name planet)
                         (get-in game-state [:current-system :name]))]
        (println (display-planet-info planet-data))
        (-> game-state
            (assoc :current-planet planet-data)
            (log-message (str "Explored planet " (:name planet)))))
      (log-message game-state "Planet not found in current system."))
    (log-message game-state "Please specify a planet to explore.")))

(defmethod execute-command "help"
  [game-state _]
  (println "\nAvailable Commands:")
  (println "  scan - Scan current star system")
  (println "  explore <planet> - Explore a specific planet")
  (println "  jump <system> - Jump to another star system")
  (println "  status - Display current status")
  (println "  help - Show this help message")
  (println "  quit - Exit game")
  game-state)

(defmethod execute-command "status"
  [game-state _]
  (let [location (get-in game-state [:player :location])]
    (println "\nCurrent Status:")
    (println "Location:" (:system location))
    (println "Coordinates:" (:coordinates location))
    (println "Fuel:" (get-in game-state [:player :fuel]))
    (println "Health:" (get-in game-state [:player :health]))
    (println "Discovered Systems:" (count (:discovered-systems game-state)))
    game-state))

(defmethod execute-command :default
  [game-state command-map]
  (log-message game-state 
               (str "Unknown command: " (:command command-map) 
                    ". Type 'help' for available commands.")))

;; Enhanced Game Loop
(defn display-game-state [game-state]
  (println "\n----------------------------------------")
  (when-let [message (last (:message-log game-state))]
    (println message))
  (println "\nLocation:" (get-in game-state [:player :location :system])
           "at" (get-in game-state [:player :location :coordinates]))
  (println "Fuel:" (get-in game-state [:player :fuel]))
  (println "\nWhat would you like to do? (Type 'help' for commands)"))

(defn game-loop [game-state]
  (display-game-state game-state)
  
  (let [input (read-line)
        command-map (parse-command input)
        new-state (execute-command game-state command-map)]
    
    (when (not= input "quit")
      (recur new-state))))

;; Start Game
(defn start-game []
  (println "\nWelcome to Space Explorer!")
  (println "What is your name, explorer?")
  (let [player-name (read-line)
        initial-state (-> initial-game-state
                         (assoc-in [:player :name] player-name)
                         (log-message "Beginning your space exploration journey..."))]
    (println "\nGreetings," player-name "!")
    (println "You start your journey in the Sol system.")
    (println "Type 'help' to see available commands.")
    (game-loop initial-state)))

;; Entry point
(defn -main [& args]
  (start-game))
