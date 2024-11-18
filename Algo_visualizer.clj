(ns user-input-example
  (:gen-class))

;;////////////////////////////////////////////////////////////////////////////////////////////////
(defn bubble-sort []
  ;; Prompt user for input
  (println "Please enter an array of space-separated integers to sort:")

  ;; Read and parse user input
  (let [user-input (read-line)
        input-array (vec (map #(Integer/parseInt %) (clojure.string/split user-input #"\s+")))]

    (println "Input array:" input-array)

    ;; Bubble sort implementation with enhanced visualization
    (let [n (count input-array)]
      (loop [arr input-array i 0 swapped true]
        (if (and swapped (< i n))
          (do
            (println (str "\nStep " (inc i) ":"))

            ;; Inner loop for comparisons and swaps
            (let [[new-arr swap]
                  (reduce (fn [[acc swapped] j]
                            (println (str "  Comparing " (acc j) " and " (acc (inc j))
                                          (if (> (acc j) (acc (inc j)))
                                            " => Swap needed!"
                                            " => No swap needed")))
                            ;; Swap elements if necessary
                            (if (> (acc j) (acc (inc j)))
                              [(assoc acc j (acc (inc j)) (inc j) (acc j)) true]
                              [acc swapped]))
                          [arr false]
                          (range 0 (- n i 1)))]

              ;; Print the array after each pass
              (println "  Array after pass:" new-arr "\n")
              (recur new-arr (inc i) swap)))
          ;; When sorting is complete
          (println "Sorted array:" arr))))))

;;////////////////////////////////////////////////////////////////////////////////////////////////
(defn insertion-sort []
  ;; Prompt user for input
  (println "Please enter an array of space-separated integers to sort:")
  
  ;; Read and parse user input
  (let [user-input (read-line)
        input-array (vec (map #(Integer/parseInt %) (clojure.string/split user-input #"\s+")))]
    
    (println "Input array:" input-array)
    
    ;; Insertion sort implementation with visualization
    (let [n (count input-array)]
      (loop [arr input-array i 1]
        (if (< i n)
          (do
            (println (str "\nStep " i ":"))
            
            ;; Initialize key (the element to be inserted) and the index j
            (let [key (arr i)
                  j (atom (dec i))
                  mutable-arr (atom arr)]
              
              ;; Move elements that are greater than the key one position ahead
              (while (and (>= @j 0) (> (@mutable-arr @j) key))
                (println (str "  Comparing key " key " with " (@mutable-arr @j) " => Shift " (@mutable-arr @j)))
                (swap! mutable-arr assoc (inc @j) (@mutable-arr @j))
                (swap! j dec))
              
              ;; Insert the key at the correct position
              (swap! mutable-arr assoc (inc @j) key)
              (println (str "  Insert key " key " at position " (inc @j)))
              (println "  Array after pass:" @mutable-arr "\n")
              
              ;; Recur with the updated array
              (recur @mutable-arr (inc i))))
          
          ;; When sorting is complete
          (println "Sorted array:" arr))))))

;;////////////////////////////////////////////////////////////////////////////////////////////////
(defn merge-sort []
  ;; Prompt user for input
  (println "Please enter an array of space-separated integers to sort:")

  ;; Read and parse user input
  (let [user-input (read-line)
        input-array (vec (map #(Integer/parseInt %) (clojure.string/split user-input #"\s+")))]

    (println "\nInput array:" input-array)

    ;; Internal merge function to merge two sorted subarrays with visualization
    (letfn [(merge [left right]
              (println "Merging:" left "and" right)
              (loop [result [] l left r right]
                (cond
                  (empty? l) (do
                               (println "Merged result:" (concat result r))
                               (concat result r))
                  (empty? r) (do
                               (println "Merged result:" (concat result l))
                               (concat result l))
                  (< (first l) (first r))
                  (recur (conj result (first l)) (rest l) r)
                  :else
                  (recur (conj result (first r)) l (rest r)))))

            ;; Recursive merge sort helper function with visualization
            (merge-sort-helper [arr]
              (let [n (count arr)]
                (if (<= n 1)
                  arr
                  (let [mid (quot n 2)
                        left (subvec arr 0 mid)
                        right (subvec arr mid n)]
                    (println "\nSplitting:" arr "into" left "and" right)
                    (merge (merge-sort-helper left) (merge-sort-helper right))))))]

      ;; Start the merge sort process and print the final sorted array
      (let [sorted-array (merge-sort-helper input-array)]
        (println "\nSorted array:" sorted-array)))))



;;////////////////////////////////////////////////////////////////////////////////////////////////
(defn quick-sort []
  ;; Prompt user for input
  (println "Please enter an array of space-separated integers to sort:")

  ;; Read and parse user input
  (let [user-input (read-line)
        input-array (vec (map #(Integer/parseInt %) (clojure.string/split user-input #"\s+")))]

    (println "\nInput array:" input-array)

    ;; Internal partition function to partition the array around the pivot with visualization
    (letfn [(partition [arr low high]
              (let [pivot (arr high)
                    index (atom low)]
                (println (str "\nPartitioning with pivot " pivot ": " arr))

                ;; Iterate and swap elements if necessary
                (doseq [j (range low high)]
                  (if (<= (arr j) pivot)
                    (do
                      (println (str "  Swapping " (arr j) " and " (arr @index)))
                      (swap! index inc)
                      (swap! index dec)
                      (let [temp (arr @index)]
                        (swap! index inc)
                        (assoc arr @index (arr j))
                        (assoc arr j temp)))))

                ;; Place pivot in the correct position
                (println (str "  Placing pivot " pivot " at index " @index))
                (assoc arr @index pivot)
                (println "  Array after partition:" arr)
                @index))

            ;; Recursive quick sort helper function with visualization
            (quick-sort-helper [arr low high]
              (if (< low high)
                (let [pi (partition arr low high)]
                  (quick-sort-helper arr low (dec pi))
                  (quick-sort-helper arr (inc pi) high))
                arr))]

      ;; Start the quick sort process and print the final sorted array
      (let [sorted-array (quick-sort-helper input-array 0 (dec (count input-array)))]
        (println "\nSorted array:" sorted-array)))))

;; To run this function, call (quick-sort)


;;////////////////////////////////////////////////////////////////////////////////////////////////
(defn heap-sort []
  ;; Prompt user for input
  (println "Please enter an array of space-separated integers to sort:")
  
  ;; Read and parse user input
  (let [user-input (read-line)
        input-array (vec (map #(Integer/parseInt %) (clojure.string/split user-input #"\s+")))]
    
    (println "\nInput array:" input-array)

    ;; Function to swap elements in the array
    (letfn [(swap [arr i j]
              (let [temp (arr i)]
                (assoc arr i (arr j) j temp)))
            
            ;; Function to heapify a subtree rooted at index `i`
            ;; `n` is the size of the heap
            (heapify [arr n i]
              (let [largest (atom i)
                    left (inc (* 2 i))
                    right (+ 2 (* 2 i))]
                (println (str "\nHeapifying at index " i ", left child: " left ", right child: " right))
                (when (and (< left n) (> (arr left) (arr @largest)))
                  (reset! largest left))
                (when (and (< right n) (> (arr right) (arr @largest)))
                  (reset! largest right))
                
                ;; If the largest is not root, swap and continue heapifying
                (if (not= @largest i)
                  (do
                    (println (str "  Swapping " (arr i) " with " (arr @largest)))
                    (let [swapped (swap arr i @largest)]
                      (heapify swapped n @largest)))
                  arr)))

            ;; Main function to perform heap sort
            (heap-sort-helper [arr]
              (let [n (count arr)]
                ;; Build the max heap
                (println "\nBuilding the max heap:")
                (loop [i (quot n 2) current-arr arr]
                  (if (>= i 0)
                    (let [new-arr (heapify current-arr n i)]
                      (recur (dec i) new-arr))
                    current-arr))

                ;; Extract elements from heap one by one
                (println "\nSorting the array:")
                (loop [i (dec n) sorted-arr arr]
                  (if (<= i 0)
                    sorted-arr
                    (let [swapped (swap sorted-arr 0 i)
                          heapified (heapify swapped i 0)]
                      (println (str "  Array after extracting max element: " heapified))
                      (recur (dec i) heapified))))))]

      ;; Start heap sort and display the sorted array
      (let [sorted-array (heap-sort-helper input-array)]
        (println "\nSorted array:" sorted-array)))))

;; To run this function, call (heap-sort)




;;////////////////////////////////////////////////////////////////////////////////////////////////
(defn binary-search []
  ;; Prompt user for input
  (println "Please enter a sorted array of space-separated integers:")
  
  ;; Read and parse the input array
  (let [user-input (read-line)
        input-array (vec (map #(Integer/parseInt %) (clojure.string/split user-input #"\s+")))]
    
    ;; Ask the user for the target value to search
    (println "Please enter the target value to search:")
    (let [target (Integer/parseInt (read-line))]
      
      (println "\nInput array:" input-array)
      (println "Target value:" target)
      
      ;; Binary search function with visualization
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

        ;; Start the binary search process
        (let [index (binary-search-helper input-array 0 (dec (count input-array)))]
          (if (not= index -1)
            (println (str "\nThe target value " target " was found at index " index))
            (println (str "\nThe target value " target " is not in the array."))))))))

;; To run this function, call (binary-search)



;;////////////////////////////////////////////////////////////////////////////////////////////////
(defn breadth-first-search []
  ;; Prompt user for input
  (println "Please enter the graph as an adjacency list (e.g., A: B C, B: D, C: E, D: , E: ):")

  ;; Read and parse the input graph
  (let [user-input (read-line)
        graph-inputs (clojure.string/split user-input #",\s*")

        ;; Parse adjacency list into a Clojure map
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

    ;; Ask the user for the starting node
    (println "\nPlease enter the starting node:")
    (let [start-node (read-line)]

      ;; BFS algorithm with visualization
      (letfn [(bfs-helper [graph start]
                (println "\nStarting Breadth-First Search:")
                (loop [queue (conj clojure.lang.PersistentQueue/EMPTY start)
                       visited #{}
                       result []]
                  (if (empty? queue)
                    (do
                      (println "\nBFS Complete. Visited Nodes:" result)
                      result)
                    (let [current (peek queue)
                          queue (pop queue)]
                      (if (contains? visited current)
                        (recur queue visited result)
                        (do
                          (println (str "  Visiting Node: " current))
                          (println (str "  Queue: " queue))
                          (let [neighbors (get graph current [])
                                new-neighbors (remove #(contains? visited %) neighbors)
                                new-queue (apply conj queue new-neighbors)]
                            (println (str "  Adding neighbors to queue: " new-neighbors))
                            (recur new-queue (conj visited current) (conj result current)))))))))]

        ;; Start BFS traversal
        (let [traversal-result (bfs-helper graph start-node)]
          (println "\nTraversal Order:" traversal-result))))))

;; To run this function, call (breadth-first-search)



;;////////////////////////////////////////////////////////////////////////////////////////////////////////
(defn depth-first-search []
  ;; Prompt user for input
  (println "Please enter the graph as an adjacency list (e.g., A: B C, B: D, C: E, D: , E: ):")

  ;; Read and parse the input graph
  (let [user-input (read-line)
        graph-inputs (clojure.string/split user-input #",\s*")

        ;; Parse adjacency list into a Clojure map
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

    ;; Ask the user for the starting node
    (println "\nPlease enter the starting node:")
    (let [start-node (read-line)]

      ;; DFS algorithm with visualization
      (letfn [(dfs-helper [graph start]
                (println "\nStarting Depth-First Search:")
                (loop [stack [start]
                       visited #{}
                       result []]
                  (if (empty? stack)
                    (do
                      (println "\nDFS Complete. Visited Nodes:" result)
                      result)
                    (let [current (peek stack)
                          stack (pop stack)]
                      (if (contains? visited current)
                        (recur stack visited result)
                        (do
                          (println (str "  Visiting Node: " current))
                          (println (str "  Stack: " stack))
                          (let [neighbors (reverse (get graph current []))
                                new-neighbors (remove #(contains? visited %) neighbors)
                                new-stack (into stack new-neighbors)]
                            (println (str "  Adding neighbors to stack: " new-neighbors))
                            (recur new-stack (conj visited current) (conj result current)))))))))]

        ;; Start DFS traversal
        (let [traversal-result (dfs-helper graph start-node)]
          (println "\nTraversal Order:" traversal-result))))))

;; To run this function, call (depth-first-search)


;;/////////////////////////////////////////////////////////////////////////////////////////
(defn selection-sort []
  ;; Prompt user for input
  (println "Please enter an array of space-separated integers to sort:")

  ;; Read and parse user input
  (let [user-input (read-line)
        input-array (vec (map #(Integer/parseInt %) (clojure.string/split user-input #"\s+")))]

    (println "\nInput array:" input-array)

    ;; Helper functions for selection sort
    (letfn [(find-min-index [arr start]
              ;; Find the index of the minimum element in the sub-array starting from index `start`
              (loop [min-idx start
                     i (inc start)]
                (if (>= i (count arr))
                  min-idx
                  (recur (if (< (arr i) (arr min-idx)) i min-idx) (inc i)))))

            (swap [arr i j]
              ;; Swap elements at indices `i` and `j` in the array
              (let [temp (arr i)]
                (assoc arr i (arr j) j temp)))

            ;; Main selection sort helper function
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
      
      ;; Execute selection sort and display the sorted array
      (let [sorted-array (selection-sort-helper input-array)]
        (println "\nSorted array:" sorted-array)))))

;; To run this function, call (selection-sort)

;;//////////////////////////////////////////////////////////////////////////////////////
(defn dijkstra []
  ;; Prompt user for input
  (println "Please enter the number of vertices:")
  (let [num-vertices (Integer/parseInt (read-line))
        
        ;; Initialize graph representation as a vector of maps
        graph (vec (repeat num-vertices {}))]

    ;; Read edges from the user
    (println "Enter the edges in the format 'source destination weight' (one per line). Enter 'done' when finished:")
    (let [graph (loop [g graph]
                  (let [input (read-line)]
                    (if (= input "done")
                      g
                      (let [[src dst weight] (map #(Integer/parseInt %) (clojure.string/split input #"\s+"))
                            updated-graph (assoc g src (assoc (get g src {}) dst weight))]
                        (recur updated-graph)))))]

      ;; Display the graph
      (println "\nGraph representation:")
      (doseq [i (range num-vertices)]
        (println (str "Vertex " i ": " (get graph i))))

      ;; Prompt user for the source vertex
      (println "\nPlease enter the source vertex:")
      (let [source (Integer/parseInt (read-line))]

        ;; Helper function to find the vertex with the minimum distance
        (letfn [(min-distance [dist visited]
                  (reduce (fn [min-idx v]
                            (if (or (contains? visited v)
                                    (and (>= (dist v) (dist min-idx))))
                              min-idx
                              v))
                          (first (remove visited (range num-vertices)))
                          (remove visited (range num-vertices))))]

          ;; Initialize distance and previous arrays
          (let [distances (vec (repeat num-vertices Double/POSITIVE_INFINITY))
                distances (assoc distances source 0)
                previous (vec (repeat num-vertices nil))
                visited #{}]

            ;; Dijkstra's algorithm visualization
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

;; To run this function, call (dijkstra)

;;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
(defn kruskal-mst []
  ;; Prompt user for input
  (println "Please enter the number of vertices:")
  (let [num-vertices (Integer/parseInt (read-line))
        
        ;; Initialize edge list
        edges []]

    ;; Read edges from the user
    (println "Enter the edges in the format 'source destination weight' (one per line). Enter 'done' when finished:")
    (let [edges (loop [e edges]
                  (let [input (read-line)]
                    (if (= input "done")
                      e
                      (let [[src dst weight] (map #(Integer/parseInt %) (clojure.string/split input #"\s+"))]
                        (recur (conj e [src dst weight]))))))]

      ;; Display the edge list
      (println "\nEdge List:")
      (doseq [edge edges]
        (println (str "Edge: " edge)))

      ;; Helper functions for Kruskal's Algorithm
      (letfn [(find [parent v]
                ;; Recursively find the root with path compression
                (let [root (get parent v v)]
                  (if (= root v)
                    v
                    (let [new-parent (find parent root)]
                      new-parent))))

              (union [parent rank u v]
                ;; Union by rank of two sets containing u and v
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

        ;; Initialize parent and rank maps for all vertices
        (let [parent (into {} (map (fn [v] [v v]) (range num-vertices)))
              rank (into {} (map (fn [v] [v 0]) (range num-vertices)))]

          ;; Sort edges by weight
          (let [sorted-edges (sort-by #(nth % 2) edges)]
            (println "\nEdges sorted by weight:")
            (doseq [edge sorted-edges]
              (println edge))

            ;; Kruskal's algorithm visualization
            (println "\nConstructing Minimum Spanning Tree using Kruskal's Algorithm:")
            (loop [mst []
                   mst-weight 0
                   edge-list sorted-edges
                   parent parent
                   rank rank]
              (if (or (empty? edge-list)
                      (= (count mst) (dec num-vertices)))
                (do
                  ;; MST construction complete
                  (println "\nMinimum Spanning Tree (MST):")
                  (doseq [edge mst]
                    (println (str "Edge: " edge)))
                  (println (str "Total weight of MST: " mst-weight))
                  [mst mst-weight])
                (let [[u v w] (first edge-list)
                      rest-edges (rest edge-list)
                      ;; Safe find for both vertices
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

;; To run this function, call (kruskal-mst)


;;///////////////////////////////////////////////////////////////////////////////////////
(defn linear-search []
  ;; Prompt user for input array
  (println "Please enter the array elements as space-separated integers:")
  (let [input-array (map #(Integer/parseInt %)
                          (clojure.string/split (read-line) #"\s+"))
        
        ;; Prompt user for the target value
        _ (println "Please enter the target value:")
        target (Integer/parseInt (read-line))]

    ;; Visualize each step of the linear search
    (println "\nStarting Linear Search:")
    (loop [index 0]
      (if (< index (count input-array))
        (let [current-value (nth input-array index)]
          ;; Display current step and comparison
          (println (str "Checking index " index " (value: " current-value ")"))

          (if (= current-value target)
            (do
              ;; If the target is found
              (println (str "Target " target " found at index " index))
              index)
            ;; Continue searching
            (recur (inc index))))
        ;; If the target is not found
        (do
          (println (str "Target " target " not found in the array"))
          -1)))))

;; To run this function, call (linear-search)

;;///////////////////////////////////////////////////////////////////
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


;;//////////////////////////////////////////////////////////////////////////////////////////////////
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
                ;; Left heavy
                (> bf 1)
                (if (< (balance-factor (:left node)) 0)
                  (-> node
                      (assoc :left (rotate-left (:left node)))
                      rotate-right)
                  (rotate-right node))
                ;; Right heavy
                (< bf -1)
                (if (> (balance-factor (:right node)) 0)
                  (-> node
                      (assoc :right (rotate-right (:right node)))
                      rotate-left)
                  (rotate-left node))
                ;; Balanced
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



;;////////////////////////////////////////////////////////////////////////////////////////////////////
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
                                     (str "[" (:value node) "]")  ; Red node: square brackets
                                     (str "(" (:value node) ")")))] ; Black node: parentheses
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

  To visualize searching algorithms and trees
  7: Binary Search
  8: Linear Search
  9: AVL Tree
  10: Binary Search Tree
  11: Breadth-First Search
  12: Depth-First Search
  13: Dijkstra's Shortest Path
  14: Kruskal's Minimum Spanning Tree
  15: Red-Black Trees")
  (let [input (read-line)]
    (cond
      (= input "1") (bubble-sort)
      (= input "2") (heap-sort)
      (= input "3") (insertion-sort)
      (= input "4") (merge-sort)
      (= input "5") (quick-sort)
      (= input "6") (selection-sort)
      (= input "7") (binary-search)
      (= input "8") (linear-search)
      (= input "9") (visualize-avl [7 4 9 1 5 8])
      (= input "10") (visualize-bst [53 22 12 34 56])
      (= input "11") (breadth-first-search)
      (= input "12") (depth-first-search) 
      (= input "13") (dijkstra) 
      (= input "14") (kruskal-mst)
      (= input "15") (visualize-rbt [42 12 87 36 11])
      :else (println "Invalid input. Please enter a number between 1 and 15 and try again."))))

;; To run the program, call the -main function
(-main)
