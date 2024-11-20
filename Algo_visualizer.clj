(ns algorithm-visualizer
  (:gen-class))

;;//////////////////////////////////////////////////BUBBLE SORT//////////////////////////////////////////////////
(defn bubble-sort-visualizer []
  (println "Please enter an array of space-separated integers to sort:")

  (let [user-input (read-line)
        input-array (vec (map #(Integer/parseInt %) (clojure.string/split user-input #"\s+")))]

    (println "\nInput array:" input-array)

    (let [n (count input-array)
          colorize (fn [text color]
                     (str "\033[" color "m" text "\033[0m")) 
          format-array (fn [arr pointer-index1 pointer-index2]
                         (apply str
                                (map-indexed
                                 (fn [idx val]
                                   (cond
                                     (= idx pointer-index1) (colorize (format "%4d" val) "31") 
                                     (= idx pointer-index2) (colorize (format "%4d" val) "31")
                                     :else (format "%4d" val)))
                                 arr)))]
      (loop [arr input-array i 0 swapped true]
        (if (and swapped (< i n))
          (do
            (println (str "\n=== Pass " (inc i) " ==="))

            (let [[new-arr swap]
                  (reduce (fn [[acc swapped] j]
                            (let [array-str (format-array acc j (inc j))
                                  pointer (apply str
                                                 (map-indexed
                                                  (fn [idx _]
                                                    (if (= idx j)
                                                      "    ^"
                                                      "     "))
                                                  acc))]
                              (println array-str)
                              (println pointer))

                            (if (> (acc j) (acc (inc j)))
                              (do
                                (println (colorize "    => Swap needed!" "33"))
                                [(assoc acc j (acc (inc j)) (inc j) (acc j)) true])
                              (do
                                (println "    => No swap needed")
                                [acc swapped])))
                          [arr false]
                          (range 0 (- n i 1)))]

              (println "\n  Array after pass:" new-arr "\n")
              (Thread/sleep 500)
              (recur new-arr (inc i) swap)))
          (do
            (println "=== Sorting Complete ===")
            (println "Sorted array:" arr)
            (println "The best-case time complexity for the bubble sort algorithm is O(n) and the worst-case time complexity for the bubble sort algorithm is O(n^2)")
            (println "The space complexity for the bubble sort algorithm is O(1)")))))))


;;//////////////////////////////////////////////////INSERTION SORT//////////////////////////////////////////////////
(defn insertion-sort-visualizer []
  (println "Please enter an array of space-separated integers to sort:")

  ;; Read and parse user input
  (let [user-input (read-line)
        input-array (vec (map #(Integer/parseInt %) (clojure.string/split user-input #"\s+")))]

    (println "\nInput array:" input-array)

    (let [n (count input-array)
          colorize (fn [text color]
                     (str "\033[" color "m" text "\033[0m"))
          format-array (fn [arr pointer-key pointer-j]
                         (apply str
                                (map-indexed
                                 (fn [idx val]
                                   (cond
                                     (= idx pointer-key) (colorize (format "%4d" val) "33")
                                     (= idx pointer-j) (colorize (format "%4d" val) "31")
                                     :else (format "%4d" val)))
                                 arr)))]
      (loop [arr input-array i 1]
        (if (< i n)
          (do
            (println (str "\n=== Step " i " ==="))
            (let [key (arr i)
                  j (atom (dec i))
                  mutable-arr (atom arr)]

              (println "Initial array:")
              (println (format-array arr i @j))

              (while (and (>= @j 0) (> (@mutable-arr @j) key))
                (println (str "  Comparing key " key " with " (@mutable-arr @j)))
                (println (format-array @mutable-arr i @j))
                (println "    => Shift" (@mutable-arr @j))
                (swap! mutable-arr assoc (inc @j) (@mutable-arr @j))
                (swap! j dec))

              (swap! mutable-arr assoc (inc @j) key)
              (println "  Insert key at position" (inc @j))
              (println (format-array @mutable-arr i nil))
              (println "  Array after pass:" @mutable-arr "\n")

              (recur @mutable-arr (inc i))))
          (do
            (println "=== Sorting Complete ===")
            (println "Sorted array:" arr)
            (println "The best-case time complexity for the insertion sort algorithm is O(n) and the worst-case time complexity for the insertion sort algorithm is O(n^2)")
            (println "The space complexity for the insertion sort algorithm is O(1)")))))))



;;//////////////////////////////////////////////////MERGE SORT//////////////////////////////////////////////////
(defn merge-sort-visualizer []
  (println "Please enter an array of space-separated integers to sort:")

  (let [user-input (read-line)
        input-array (vec (map #(Integer/parseInt %) (clojure.string/split user-input #"\s+")))]

    (println "\nInput array:" input-array)

    (letfn [(colorize [text color]
              (str "\033[" color "m" text "\033[0m")) 
            (format-array [arr]
              (apply str (map #(format "%4d" %) arr)))
            (merge [left right]
              (println "\nMerging:")
              (println "Left : " (format-array left))
              (println "Right: " (format-array right))

              (loop [result [] l left r right]
                (cond
                  (empty? l)
                  (do
                    (println "  Remaining right appended: " (format-array r))
                    (println "  Merged result: " (format-array (concat result r)))
                    (concat result r))

                  (empty? r)
                  (do
                    (println "  Remaining left appended: " (format-array l))
                    (println "  Merged result: " (format-array (concat result l)))
                    (concat result l))

                  (< (first l) (first r))
                  (do
                    (println "  Comparing: "
                             (colorize (format "%d" (first l)) "33")
                             "and"
                             (colorize (format "%d" (first r)) "31"))
                    (println "    => Taking: " (first l))
                    (recur (conj result (first l)) (rest l) r))

                  :else
                  (do
                    (println "  Comparing: "
                             (colorize (format "%d" (first l)) "33")
                             "and"
                             (colorize (format "%d" (first r)) "31"))
                    (println "    => Taking: " (first r))
                    (recur (conj result (first r)) l (rest r))))))

            (merge-sort-helper [arr]
              (let [n (count arr)]
                (if (<= n 1)
                  arr
                  (let [mid (quot n 2)
                        left (subvec arr 0 mid)
                        right (subvec arr mid n)]
                    (println "\nSplitting:")
                    (println "Array : " (format-array arr))
                    (println "Into   : " (format-array left) "and" (format-array right))
                    (merge (merge-sort-helper left) (merge-sort-helper right))))))]

      (let [sorted-array (merge-sort-helper input-array)]
        (println "\n=== Sorting Complete ===")
        (println "Sorted array:" (format-array sorted-array))
        (println "The best-case time complexity for the merge sort algorithm is O(n log n) and the worst-case time complexity for the merge sort algorithm is O(n log n)")
        (println "The space complexity for the merge sort algorithm is O(n)")))))




;;//////////////////////////////////////////////////QUICK SORT//////////////////////////////////////////////////
(defn quick-sort-visualizer []
  (println "Please enter an array of space-separated integers to sort:")

  (let [user-input (read-line)
        input-array (vec (map #(Integer/parseInt %) (clojure.string/split user-input #"\s+")))]

    (println "\nInput array:" input-array)

    (letfn [(colorize [text color]
              (str "\033[" color "m" text "\033[0m"))
            (format-array [arr pivot-index]
              (map-indexed (fn [idx val]
                             (if (= idx pivot-index)
                               (colorize (str val) "33")
                               (str val))) arr))

            (partition [arr low high]
              (let [pivot (arr high)
                    index (atom low)
                    mutable-arr (atom arr)]
                (println "\nPartitioning:")
                (println "Array : " (str (format-array @mutable-arr high)))
                (println "Pivot : " pivot)

                (doseq [j (range low high)]
                  (if (<= (@mutable-arr j) pivot)
                    (do
                      (println (str "  Comparing " (@mutable-arr j) " <= " pivot " => True"))
                      (let [temp (@mutable-arr @index)]
                        (swap! mutable-arr assoc @index (@mutable-arr j))
                        (swap! mutable-arr assoc j temp)
                        (println "  Swapping:" (format-array @mutable-arr high)))
                      (swap! index inc))
                    (println (str "  Comparing " (@mutable-arr j) " <= " pivot " => False"))))

                (let [temp (@mutable-arr @index)]
                  (swap! mutable-arr assoc @index pivot)
                  (swap! mutable-arr assoc high temp))
                (println "  Placing pivot:" pivot "at index" @index)
                (println "  Array after partition:" (format-array @mutable-arr @index))
                [@mutable-arr @index]))

            (quick-sort-helper [arr low high]
              (if (< low high)
                (let [[parted-arr pi] (partition arr low high)]
                  (println (str "\nRecursively sorting left side: [" low ", " (dec pi) "]"))
                  (let [left-sorted (quick-sort-helper parted-arr low (dec pi))]
                    (println (str "\nRecursively sorting right side: [" (inc pi) ", " high "]"))
                    (quick-sort-helper left-sorted (inc pi) high)))
                arr))]

      (let [sorted-array (quick-sort-helper input-array 0 (dec (count input-array)))]
        (println "\n=== Sorting Complete ===")
        (println "Sorted array:" sorted-array)
        (println "The best-case time complexity for the quick sort algorithm is O(n log n) and the worst-case time complexity for the quick sort algorithm is O(n^2)")
        (println "The space complexity for the quick sort algorithm is O(log n)")))))



;;//////////////////////////////////////////////////HEAP SORT//////////////////////////////////////////////////
(defn heap-sort [] 
  (println "Please enter an array of space-separated integers to sort:")

  (let [user-input (read-line)
        input-array (vec (map #(Integer/parseInt %) (clojure.string/split user-input #"\s+")))]

    (println "\nInput array:" input-array)

    (letfn [(swap [arr i j]
              (let [temp (arr i)]
                (assoc arr i (arr j) j temp)))

          
            (heapify [arr n i]
              (let [largest (atom i)
                    left (inc (* 2 i))
                    right (+ 2 (* 2 i))]
                (println (str "\nHeapifying at index " i " (current root: " (arr i) "), left: "
                              (if (< left n) (str (arr left)) "N/A")
                              ", right: "
                              (if (< right n) (str (arr right)) "N/A")))

                (when (and (< left n) (> (arr left) (arr @largest)))
                  (println (str "  Left child " (arr left) " is greater than root " (arr @largest)))
                  (reset! largest left))

                (when (and (< right n) (> (arr right) (arr @largest)))
                  (println (str "  Right child " (arr right) " is greater than current largest " (arr @largest)))
                  (reset! largest right))

                (if (not= @largest i)
                  (do
                    (println (str "  Swapping " (arr i) " with " (arr @largest)))
                    (let [swapped (swap arr i @largest)]
                      (heapify swapped n @largest)))
                  arr)))

            (heap-sort-helper [arr]
              (let [n (count arr)]
                (println "\nBuilding the max heap:")
                (loop [i (dec (quot n 2)) current-arr arr]
                  (if (>= i 0)
                    (let [new-arr (heapify current-arr n i)]
                      (recur (dec i) new-arr))
                    current-arr))

                (println "\nSorting the array:")
                (loop [i (dec n) sorted-arr arr]
                  (if (<= i 0)
                    sorted-arr
                    (let [swapped (swap sorted-arr 0 i)
                          heapified (heapify swapped i 0)]
                      (println (str "  Array after extracting max element: " heapified))
                      (recur (dec i) heapified))))))]

      (let [sorted-array (heap-sort-helper input-array)]
        (println "\nSorted array:" sorted-array)
        (println "The best-case time complexity for the heap sort algorithm is O(n log n) and the worst-case time complexity for the heap sort algorithm is O(n log n)")
        (println "The space complexity for the heap sort algorithm is O(1)")))))



;;//////////////////////////////////////////////////BINARY-SEARCH//////////////////////////////////////////////////
(defn binary-search []
  (println "Please enter a sorted array of space-separated integers:")

  (let [user-input (read-line)
        input-array (vec (map #(Integer/parseInt %) (clojure.string/split user-input #"\s+")))]

    (println "Please enter the target value to search:")
    (let [target (Integer/parseInt (read-line))]

      (println "\nInput array:" input-array)
      (println "Target value:" target)

      (letfn [(binary-search-helper [arr left right]
                (if (<= left right)
                  (let [mid (quot (+ left right) 2)
                        mid-value (arr mid)]
                    (println (str "\nSearching between indexes " left " and " right))
                    (println (str "  Middle index: " mid ", Middle value: " mid-value))

                    (cond
                      (= mid-value target)
                      (do
                        (println (str "  Target found at index " mid))
                        mid)

                      (< mid-value target)
                      (do
                        (println (str "  Target is greater than " mid-value))
                        (binary-search-helper arr (inc mid) right))

                      (> mid-value target)
                      (do
                        (println (str "  Target is less than " mid-value))
                        (binary-search-helper arr left (dec mid)))))

                  (do
                    (println "  Target not found in the array.")
                    -1)))]

        (let [index (binary-search-helper input-array 0 (dec (count input-array)))]
          (if (not= index -1)
            (println (str "\nThe target value " target " was found at index " index))
            (println (str "\nThe target value " target " is not in the array."))))))))




;;//////////////////////////////////////////////////BREADTH-FIRST-SEARCH//////////////////////////////////////////////////
(defn visualize-bfs []
  (println "Welcome to the BFS Visualizer!")
  (println "Please enter the graph as an adjacency list (e.g., A: B C, B: D, C: E, D: , E: ):") 

  (let [user-input (read-line)
        graph-inputs (clojure.string/split user-input #",\s*")

        graph (reduce (fn [acc input]
                        (let [[node neighbors] (clojure.string/split input #":\s*")
                              neighbors-list (if (empty? neighbors)
                                               []
                                               (clojure.string/split neighbors #"\s+"))]
                          (assoc acc node neighbors-list)))
                      {} graph-inputs)]

    (println "\nParsed Graph:")
    (doseq [[node neighbors] graph]
      (println (str "  " node " -> " neighbors)))

    (println "\nPlease enter the starting node:")
    (let [start-node (read-line)]

      (println "\nStarting BFS Visualization...")
      (loop [queue (conj clojure.lang.PersistentQueue/EMPTY start-node)
             visited #{}
             pointer ""]
        (if (empty? queue)
          (println "\nBFS Complete!")
          (let [current (peek queue)
                queue (pop queue)]
            (if (contains? visited current)
              (recur queue visited pointer)
              (do
                (let [pointer (str " -> [" current "]")
                      neighbors (get graph current [])
                      new-neighbors (remove #(contains? visited %) neighbors)
                      new-queue (apply conj queue new-neighbors)]
                  (println (str "\nPointer:" pointer))
                  (println (str "  Visiting Node: " current))
                  (println (str "  Neighbors: " neighbors))
                  (println (str "  Adding to queue: " new-neighbors))
                  (println (str "  Current Queue: " (vec new-queue)))

                  (recur new-queue (conj visited current) pointer))))))))))





;;//////////////////////////////////////////////////DEPTH-FIRST-SEARCH//////////////////////////////////////////////////
(defn visualize-dfs []
  (println "Welcome to the DFS Visualizer!")
  (println "Please enter the graph as an adjacency list (e.g., A: B C, B: D, C: E, D: , E: ):")

  (let [user-input (read-line)
        graph-inputs (clojure.string/split user-input #",\s*")

        graph (reduce (fn [acc input]
                        (let [[node neighbors] (clojure.string/split input #":\s*")
                              neighbors-list (if (empty? neighbors)
                                               []
                                               (clojure.string/split neighbors #"\s+"))]
                          (assoc acc node neighbors-list)))
                      {} graph-inputs)]

    (println "\nParsed Graph:")
    (doseq [[node neighbors] graph]
      (println (str "  " node " -> " neighbors)))

    (println "\nPlease enter the starting node:")
    (let [start-node (read-line)]

      (println "\nStarting DFS Visualization...")
      (loop [stack [start-node]
             visited #{}
             pointer ""]
        (if (empty? stack)
          (println "\nDFS Complete!")
          (let [current (peek stack)
                stack (pop stack)]
            (if (contains? visited current)
              (recur stack visited pointer)
              (do
                (let [pointer (str " -> [" current "]")
                      neighbors (get graph current [])
                      new-neighbors (remove #(contains? visited %) neighbors)
                      new-stack (into stack (reverse new-neighbors))]
                  (println (str "\nPointer:" pointer))
                  (println (str "  Visiting Node: " current))
                  (println (str "  Neighbors: " neighbors))
                  (println (str "  Adding to stack: " new-neighbors))
                  (println (str "  Current Stack: " (vec new-stack)))

                  (recur new-stack (conj visited current) pointer))))))))))


;;//////////////////////////////////////////////////SELECTION SORT//////////////////////////////////////////////////
(defn selection-sort []
  (println "Please enter an array of space-separated integers to sort:")

  (let [user-input (read-line)
        input-array (vec (map #(Integer/parseInt %) (clojure.string/split user-input #"\s+")))]

    (println "\nInput array:" input-array)

    (letfn [(find-min-index [arr start]
              (loop [min-idx start
                     i (inc start)]
                (if (>= i (count arr))
                  min-idx
                  (recur (if (< (arr i) (arr min-idx)) i min-idx) (inc i)))))

            (swap [arr i j]
              (let [temp (arr i)]
                (assoc arr i (arr j) j temp)))

            (selection-sort-helper [arr]
              (println "\nPerforming Selection Sort:")
              (loop [i 0
                     sorted-arr arr]
                (if (>= i (dec (count sorted-arr)))
                  sorted-arr
                  (let [min-idx (find-min-index sorted-arr i)
                        new-arr (if (not= min-idx i)
                                  (do
                                    (println (str "  Swapping " (sorted-arr i) " and " (sorted-arr min-idx)))
                                    (swap sorted-arr i min-idx))
                                  sorted-arr)]
                    (println (str "  Array after step " (inc i) ": " new-arr))
                    (recur (inc i) new-arr)))))]
      
      (let [sorted-array (selection-sort-helper input-array)]
        (println "\nSorted array:" sorted-array)
        (println "The best-case time complexity for the selection sort algorithm is O(n^2) and the worst-case time complexity for the selection sort algorithm is O(n^2)")
        (println "The space complexity for the selection sort algorithm is O(1)")))))

;;//////////////////////////////////////////////////DIJKSTRA'S SHORTEST PATH//////////////////////////////////////////////////
(defn dijkstra []
  (println "Please enter the number of vertices:")
  (let [num-vertices (Integer/parseInt (read-line))
        
        graph (vec (repeat num-vertices {}))]

    (println "Enter the edges in the format 'source destination weight' (one per line). Enter 'done' when finished:")
    (let [graph (loop [g graph]
                  (let [input (read-line)]
                    (if (= input "done")
                      g
                      (let [[src dst weight] (map #(Integer/parseInt %) (clojure.string/split input #"\s+"))
                            updated-graph (assoc g src (assoc (get g src {}) dst weight))]
                        (recur updated-graph)))))]

      (println "\nGraph representation:")
      (doseq [i (range num-vertices)]
        (println (str "Vertex " i ": " (get graph i))))

      (println "\nPlease enter the source vertex:")
      (let [source (Integer/parseInt (read-line))]

        (letfn [(min-distance [dist visited]
                  (reduce (fn [min-idx v]
                            (if (or (contains? visited v)
                                    (and (>= (dist v) (dist min-idx))))
                              min-idx
                              v))
                          (first (remove visited (range num-vertices)))
                          (remove visited (range num-vertices))))]

          (let [distances (vec (repeat num-vertices Double/POSITIVE_INFINITY))
                distances (assoc distances source 0)
                previous (vec (repeat num-vertices nil))
                visited #{}]

            (println "\nRunning Dijkstra's Algorithm:")
            (loop [dist distances
                   prev previous
                   visited visited]
              (if (= (count visited) num-vertices)
                (do
                  (println "\nFinal shortest distances from source vertex:" source)
                  (doseq [i (range num-vertices)]
                    (println (str "  Vertex " i ": Distance = " (dist i) ", Previous = " (prev i))))
                  [dist prev])
                (let [u (min-distance dist visited)
                      neighbors (get graph u)]
                  (println (str "\nVisiting vertex " u ", current distance: " (dist u)))
                  (let [updated-dist (reduce (fn [d [v w]]
                                                (if (not (contains? visited v))
                                                  (let [alt (+ (d u) w)]
                                                    (if (< alt (d v))
                                                      (do
                                                        (println (str "  Updating distance of vertex " v " from " (d v) " to " alt))
                                                        (assoc d v alt))
                                                      d))
                                                  d))
                                              dist
                                              neighbors)

                        updated-prev (reduce (fn [p [v w]]
                                               (if (and (not (contains? visited v))
                                                        (< (+ (dist u) w) (dist v)))
                                                 (assoc p v u)
                                                 p))
                                             prev
                                             neighbors)]
                    (recur updated-dist updated-prev (conj visited u))))))))))))


;;//////////////////////////////////////////////////KRUSKAL'S MINIMUM SPANNING TREE//////////////////////////////////////////////////
(defn kruskal-mst []
  (println "Please enter the number of vertices:")
  (let [num-vertices (Integer/parseInt (read-line))
        
        edges []]

    (println "Enter the edges in the format 'source destination weight' (one per line). Enter 'done' when finished:")
    (let [edges (loop [e edges]
                  (let [input (read-line)]
                    (if (= input "done")
                      e
                      (let [[src dst weight] (map #(Integer/parseInt %) (clojure.string/split input #"\s+"))]
                        (recur (conj e [src dst weight]))))))]

      (println "\nEdge List:")
      (doseq [edge edges]
        (println (str "Edge: " edge)))

      (letfn [(find [parent v]
                (let [root (get parent v v)]
                  (if (= root v)
                    v
                    (let [new-parent (find parent root)]
                      new-parent))))

              (union [parent rank u v]
                (let [root-u (find parent u)
                      root-v (find parent v)]
                  (if (not= root-u root-v)
                    (if (< (get rank root-u 0) (get rank root-v 0))
                      [(assoc parent root-u root-v) rank]
                      (if (> (get rank root-u 0) (get rank root-v 0))
                        [(assoc parent root-v root-u) rank]
                        [(assoc parent root-v root-u)
                         (assoc rank root-u (inc (get rank root-u 0)))]))
                    [parent rank])))]

        (let [parent (into {} (map (fn [v] [v v]) (range num-vertices)))
              rank (into {} (map (fn [v] [v 0]) (range num-vertices)))]

          (let [sorted-edges (sort-by #(nth % 2) edges)]
            (println "\nEdges sorted by weight:")
            (doseq [edge sorted-edges]
              (println edge))

            (println "\nConstructing Minimum Spanning Tree using Kruskal's Algorithm:")
            (loop [mst []
                   mst-weight 0
                   edge-list sorted-edges
                   parent parent
                   rank rank]
              (if (or (empty? edge-list)
                      (= (count mst) (dec num-vertices)))
                (do
                  (println "\nMinimum Spanning Tree (MST):")
                  (doseq [edge mst]
                    (println (str "Edge: " edge)))
                  (println (str "Total weight of MST: " mst-weight))
                  [mst mst-weight])
                (let [[u v w] (first edge-list)
                      rest-edges (rest edge-list)
                      root-u (find parent u)
                      root-v (find parent v)]
                  (if (not= root-u root-v)
                    (let [[new-parent new-rank] (union parent rank u v)]
                      (println (str "Adding edge " [u v w] " to MST"))
                      (recur (conj mst [u v w])
                             (+ mst-weight w)
                             rest-edges
                             new-parent
                             new-rank))
                    (do
                      (println (str "Skipping edge " [u v w] " (would form a cycle)"))
                      (recur mst mst-weight rest-edges parent rank))))))))))))



;;//////////////////////////////////////////////////LINEAR SEARCH//////////////////////////////////////////////////
(defn linear-search []
  (println "Please enter the array elements as space-separated integers:")
  (let [input-array (map #(Integer/parseInt %)
                          (clojure.string/split (read-line) #"\s+"))
        
        _ (println "Please enter the target value:")
        target (Integer/parseInt (read-line))]

    (println "\nStarting Linear Search:")
    (loop [index 0]
      (if (< index (count input-array))
        (let [current-value (nth input-array index)]
          (println (str "Checking index " index " (value: " current-value ")"))

          (if (= current-value target)
            (do
              (println (str "Target " target " found at index " index))
              index)
            (recur (inc index))))
        (do
          (println (str "Target " target " not found in the array"))
          -1)))))


;;//////////////////////////////////////////////////BINARY SEARCH TREE//////////////////////////////////////////////////
(defn visualize-bst
  "Creates a binary search tree and visualizes its construction step by step,
   including the array and the tree's structure."
  [array]
  (letfn [(insert [tree value]
            (if tree
              (if (< value (:value tree))
                (assoc tree :left (insert (:left tree) value))
                (assoc tree :right (insert (:right tree) value)))
              {:value value :left nil :right nil}))
          (tree->string [tree depth]
            (if tree
              (let [left  (tree->string (:left tree) (inc depth))
                    right (tree->string (:right tree) (inc depth))
                    value-str (str (apply str (repeat (* 2 depth) " ")) (:value tree))]
                (str left "\n" value-str "\n" right))
              ""))
          (sorted-array [tree]
            (if tree
              (concat (sorted-array (:left tree))
                      [(:value tree)]
                      (sorted-array (:right tree)))
              []))
          (step-visualize [tree current remaining]
            (println "\n--- Step Visualization ---")
            (println "Array being sorted: " remaining)
            (println "Current value:     " current)
            (println "Binary Search Tree:")
            (println (tree->string tree 0))
            (println "Sorted array so far:" (sorted-array tree))
            (println "-------------------------\n"))]
    (loop [remaining array
           tree nil]
      (if (seq remaining)
        (let [current (first remaining)]
          (step-visualize tree current (rest remaining))
          (recur (rest remaining) (insert tree current)))
        (do
          (println "Final Tree Visualization:")
          (println (tree->string tree 0))
          (println "Final Sorted Array:" (sorted-array tree))
          tree)))))


;;//////////////////////////////////////////////////AVL TREE//////////////////////////////////////////////////
(defn visualize-avl
  "Creates an AVL tree and visualizes its construction step by step,
   including the array, the tree's structure, and balance adjustments."
  [array]
  (letfn [(height [node]
            (if node
              (:height node)
              0))
          (balance-factor [node]
            (- (height (:left node)) (height (:right node))))
          (update-height [node]
            (assoc node :height (inc (max (height (:left node))
                                          (height (:right node))))))
          (rotate-right [node]
            (let [left (:left node)]
              (-> left
                  (assoc :right (assoc node :left (:right left)))
                  update-height
                  (assoc-in [:right] (update-height (:right left))))))
          (rotate-left [node]
            (let [right (:right node)]
              (-> right
                  (assoc :left (assoc node :right (:left right)))
                  update-height
                  (assoc-in [:left] (update-height (:left right))))))
          (balance [node]
            (let [bf (balance-factor node)]
              (cond 
                (> bf 1)
                (if (< (balance-factor (:left node)) 0)
                  (-> node
                      (assoc :left (rotate-left (:left node)))
                      rotate-right)
                  (rotate-right node))
                (< bf -1)
                (if (> (balance-factor (:right node)) 0)
                  (-> node
                      (assoc :right (rotate-right (:right node)))
                      rotate-left)
                  (rotate-left node))
                :else (update-height node))))
          (insert [tree value]
            (if tree
              (let [tree (if (< value (:value tree))
                           (assoc tree :left (insert (:left tree) value))
                           (assoc tree :right (insert (:right tree) value)))]
                (balance tree))
              {:value value :left nil :right nil :height 1}))
          (tree->string [tree depth]
            (if tree
              (let [left  (tree->string (:left tree) (inc depth))
                    right (tree->string (:right tree) (inc depth))
                    value-str (str (apply str (repeat (* 2 depth) " ")) (:value tree))]
                (str left "\n" value-str "\n" right))
              ""))
          (sorted-array [tree]
            (if tree
              (concat (sorted-array (:left tree))
                      [(:value tree)]
                      (sorted-array (:right tree)))
              []))
          (step-visualize [tree current remaining]
            (println "\n--- Step Visualization ---")
            (println "Array being sorted: " remaining)
            (println "Current value:     " current)
            (println "AVL Tree:")
            (println (tree->string tree 0))
            (println "Sorted array so far:" (sorted-array tree))
            (println "-------------------------\n"))]
    (loop [remaining array
           tree nil]
      (if (seq remaining)
        (let [current (first remaining)]
          (step-visualize tree current (rest remaining))
          (recur (rest remaining) (insert tree current)))
        (do
          (println "Final Tree Visualization:")
          (println (tree->string tree 0))
          (println "Final Sorted Array:" (sorted-array tree))
          tree)))))



;;//////////////////////////////////////////////////RED-BLACK TREE//////////////////////////////////////////////////
(defn visualize-rbt
  "Creates a Red-Black Tree (RBT) and visualizes its construction step by step,
   including the array, tree structure, and color balancing."
  [array]
  (letfn [(make-node [value color left right]
            {:value value :color color :left left :right right})
          (is-red? [node]
            (and node (= (:color node) :red)))
          (rotate-left [node]
            (let [right (:right node)]
              (make-node (:value right) (:color node)
                         (make-node (:value node) :red (:left node) (:left right))
                         (:right right))))
          (rotate-right [node]
            (let [left (:left node)]
              (make-node (:value left) (:color node)
                         (:left left)
                         (make-node (:value node) :red (:right left) (:right node)))))
          (flip-colors [node]
            (-> node
                (assoc :color :red)
                (update :left assoc :color :black)
                (update :right assoc :color :black)))
          (balance [node]
            (cond
              (is-red? (:right node)) (rotate-left node)
              (and (is-red? (:left node)) (is-red? (:left (:left node)))) (rotate-right node)
              (and (is-red? (:left node)) (is-red? (:right node))) (flip-colors node)
              :else node))
          (insert [node value]
            (if node
              (let [node (if (< value (:value node))
                           (update node :left insert value)
                           (update node :right insert value))]
                (balance node))
              (make-node value :red nil nil)))
          (ensure-black-root [node]
            (if node (assoc node :color :black) nil))
          (tree->string [node depth]
            (if node
              (let [left (tree->string (:left node) (inc depth))
                    right (tree->string (:right node) (inc depth))
                    value-str (str (apply str (repeat (* 2 depth) " "))
                                   (if (= (:color node) :red)
                                     (str "[" (:value node) "]")
                                     (str "(" (:value node) ")")))]
                (str left "\n" value-str "\n" right))
              ""))
          (sorted-array [node]
            (if node
              (concat (sorted-array (:left node))
                      [(:value node)]
                      (sorted-array (:right node)))
              []))
          (step-visualize [tree current remaining]
            (println "\n--- Step Visualization ---")
            (println "Array being sorted: " remaining)
            (println "Current value:     " current)
            (println "Red-Black Tree:")
            (println (tree->string tree 0))
            (println "Sorted array so far:" (sorted-array tree))
            (println "-------------------------\n"))]
    (loop [remaining array
           tree nil]
      (if (seq remaining)
        (let [current (first remaining)]
          (step-visualize tree current (rest remaining))
          (recur (rest remaining) (ensure-black-root (insert tree current))))
        (do
          (println "Final Tree Visualization:")
          (println (tree->string tree 0))
          (println "Final Sorted Array:" (sorted-array tree))
          tree)))))



(defn -main []
  (println "Enter a number for the algorithm of your choice: 
  To visualize a sorting algorithm:
  1: Bubble Sort
  2: Heap Sort
  3: Insertion Sort
  4: Merge Sort
  5: Quick Sort
  6: Selection Sort

  To visualize searching algorithms:
  7: Binary Search
  8: Linear Search
  9: Breadth-First Search
  10: Depth-First Search
  11: Dijkstra's Shortest Path
  
  To visualize Tree Algorithms:
  12: AVL Tree
  13: Binary Search Tree
  14: Kruskal's Minimum Spanning Tree
  15: Red-Black Tree")
  (let [input (read-line)]
    (cond
      (= input "1") (bubble-sort-visualizer)
      (= input "2") (heap-sort)
      (= input "3") (insertion-sort-visualizer)
      (= input "4") (merge-sort-visualizer)
      (= input "5") (quick-sort-visualizer)
      (= input "6") (selection-sort)
      (= input "7") (binary-search)
      (= input "8") (linear-search)
      (= input "9") (visualize-bfs)
      (= input "10") (visualize-dfs)
      (= input "11") (dijkstra)
      (= input "12") (visualize-avl [7 4 9 1 5 8]) 
      (= input "13") (visualize-bst [53 22 12 34 56]) 
      (= input "14") (kruskal-mst)
      (= input "15") (visualize-rbt [42 12 87 36 11])
      :else (println "Invalid input. Please enter a number between 1 and 15 and try again."))))

(-main)
