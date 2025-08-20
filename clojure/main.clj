; Work in progress

(def rows 6)
(def cols 7)

(defn new-board []
  (vec (repeat rows (vec (repeat cols 0)))))

(defn token->ch [t]
  (case t
    1 \X
    -1 \O
    \.))

(defn print-board [board]
  (println)
  (doseq [r board]
    (println (apply str (map #(str (token->ch %) " ") r))))
  (println (apply str (map #(str (inc %) " ") (range cols)))))

(defn column-full? [board c]
  (not (zero? (get-in board [0 c]))))

(defn drop-piece [board c player]
  (when-not (column-full? board c)
    (let [r (last (filter #(zero? (get-in board [% c])) (range rows)))]
      (assoc-in board [r c] player))))

(def directions [[0 1] [1 0] [1 1] [1 -1]])

(defn in-bounds? [r c]
  (and (<= 0 r) (< r rows) (<= 0 c) (< c cols)))

(defn line-owner [board r c dr dc]
  (let [t (get-in board [r c])]
    (when (not (zero? t))
      (let [coords (for [k (range 4)] [(+ r (* k dr)) (+ c (* k dc))])]
        (when (every? (fn [[rr cc]]
                        (and (in-bounds? rr cc)
                             (= t (get-in board [rr cc]))))
                      coords)
          t)))))

(defn winner [board]
  (some (fn [r]
          (some (fn [c]
                  (some (fn [[dr dc]] (line-owner board r c dr dc))
                        directions))
                (range cols)))
        (range rows)))

(defn board-full? [board]
  (every? #(not (zero? (first %))) board))

(defn play-turn [board player]
  (loop []
    (println (format "Player %s, choose column 1-%d (or q to quit):"
                     (if (= player 1) "X" "O") cols))
    (print "> ") (flush)
    (let [s (clojure.string/trim (read-line))]
      (cond
        (#{"q" "Q"} s) :quit
        :else
        (let [mv (dec (try (Integer/parseInt s) (catch Exception _ -1)))]
          (cond
            (or (< mv 0) (>= mv cols)) (do (println "Out of range.") (recur))
            (column-full? board mv) (do (println "Column full.") (recur))
            :else (drop-piece board mv player))))))))

(defn game-loop []
  (loop [board (new-board) player 1]
    (print-board board)
    (if-let [w (winner board)]
      (println (format "\nGame over! %s wins."
                       (if (= w 1) "Player X" "Player O")))
      (if (board-full? board)
        (println "\nIt's a draw!")
        (let [res (play-turn board player)]
          (if (= res :quit)
            (println "Goodbye!")
            (recur res (- player))))))))

(defn -main []
  (println "Welcome to Connect Four! Two players: X and O")
  (game-loop))

(-main)
