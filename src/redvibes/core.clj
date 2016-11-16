(ns redvibes.core
  "Simple implementation of the Red Vibes game."
  (use [clojure.test :only [is]]
       [test.core :onld [is=]]))

(defn
  ^{:doc "Creates a shuffeled deck consisting of 26 black & red cards and one joker.
          (The deck does not contain any suits, numbers or face cards.)"}
  create-deck []
  (->
    (list "joker")
    (concat (->>
              (repeat "red")
              (take 26)))
    (concat (->>
              (repeat "black")
              (take 26)))
    (shuffle)))

(defn
  ^{:doc "Creates the state."}
  create-state []
  {:deck            (create-deck)
   :drawn-cards     (list)
   :pick-last-drawn false})

(defn
  ^{:doc  "Draw a card from the deck adding it to the list of drawn cards."
    :test (fn []
            (is= (draw-card {:deck        (list "black" "black" "red")
                             :drawn-cards (list "red")})
                 {:deck        (list "black" "red")
                  :drawn-cards (list "black" "red")}))}
  draw-card [state]
  (let [drawn-card (-> (:deck state) (peek))]
    (->
      (update-in state [:drawn-cards] conj drawn-card)
      (update :deck pop))))

(defn
  ^{:doc  "Determines if there are any cards left in the deck."
    :test (fn []
            (is= (no-cards-left? {:deck (list)})
                 true)
            (is= (no-cards-left? {:deck (list "black")})
                 false))}
  no-cards-left? [state]
  (empty? (:deck state)))

(defn
  ^{:doc  "Determines if there are more drawn black cards than red cards."
    :test (fn []
            (is= (more-black-drawn? {:drawn-cards (list "red")})
                 false)
            (is= (more-black-drawn? {:drawn-cards (list "black" "red" "black")})
                 true)
            (is= (more-black-drawn? {:drawn-cards (list "red" "black")})
                 false)
            )}
  more-black-drawn? [state]
  (let [freq (->>
               (:drawn-cards state)
               (frequencies)
               (sort-by val)
               (reverse))]
    (if (and (->>
               (take 1 freq)
               (map first)
               (= (list "black")))
             (not (= (second (first freq)) (second (second freq)))))
      true
      false)
    ))

(defn
  ^{:doc  "Plays one turn of the game."
    :test (fn []
            (is= (play {:deck            (list)
                        :drawn-cards     (list "red")
                        :pick-last-drawn false})
                 {:deck            (list)
                  :drawn-cards     (list "red")
                  :pick-last-drawn true})
            (is= (play {:deck            (list "red")
                        :drawn-cards     (list "black")
                        :pick-last-drawn false})
                 {:deck            (list)
                  :drawn-cards     (list "red" "black")
                  :pick-last-drawn true})
            (is= (play {:deck            (list "red")
                        :drawn-cards     (list)
                        :pick-last-drawn false})
                 {:deck            (list)
                  :drawn-cards     (list "red")
                  :pick-last-drawn false})
            )}
  play [state]
  (if (no-cards-left? state)
    (conj state {:pick-last-drawn true})
    (if (more-black-drawn? state)
      (->
        (draw-card state)
        (conj {:pick-last-drawn true}))
      (draw-card state))))

(defn
  ^{:doc  "Determines if the game has reached an end state."
    :test (fn []
            (is= (end? {:pick-last-drawn true})
                 true)
            (is= (end? {:pick-last-drawn false})
                 false))}
  end? [state]
  (:pick-last-drawn state))

(defn
  ^{:doc  "Calculates the return of the game.
           0 SEK if picked card is black and 200 SEK if picked card is red."
    :test (fn []
            (is= (calculate-return {:drawn-cards (list "red")})
                 200)
            (is= (calculate-return {:drawn-cards (list "black")})
                 0))}
  calculate-return [state]
  (let [picked-card (->>
                      (:drawn-cards state)
                      (peek))]
    (if (= picked-card "red")
      200
      0)))

(defn -main
  "Playes a game of Red Vibes."
  [& args]
  (def state (atom (create-state)))
  (while (not (:pick-last-drawn @state))
    (do
      (swap! state play)))
  (calculate-return @state))