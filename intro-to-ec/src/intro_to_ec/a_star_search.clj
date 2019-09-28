(ns intro-to-ec.a-star-search
  (:require [clojure.set :as cset]
            [clojure.data.priority-map :as pm]))

(defn remove-previous-states
  [new-states frontier visited]
  (remove (cset/union (set (keys frontier)) (set visited)) new-states))

(def depth-first-search
  {:get-next-node first
   :add-children concat})

(def breadth-first-search
  {:get-next-node first
   :add-children #(concat %2 %1)})

(def random-search
  {:get-next-node rand-nth
   :add-children concat})

(def heuristic-search
  {:get-next-node (fn [n] (first(first n)))
   :add-children into})

   (def a-star-search
     {:get-next-node (fn [n] (first(first n)))
      :add-children into})

(defn generate-cost
  [new-nodes current-node cost-so-far]
  ()
  )

(defn generate-path
  [came-from node]
  (if (= :start-node (get came-from node))
    [node]
    (conj (generate-path came-from (get came-from node)) node)))

(defn search
  [{:keys [get-enext-node add-children]}
   {:keys [goal? make-children heuristic]}
   start-node
   max-calls
   ]
  (let [goal [0 0]]
  (loop [frontier (pm/priority-map start-node (heuristic start-node goal)),
         came-from {start-node :start-node},
         cost-so-far {start-node 0}
         num-calls 0 ]
    (println num-calls ": " frontier)
    (println came-from)
    (let [current-node (get-next-node frontier)]
      (cond
        (goal? current-node) (generate-path came-from current-node)
        (= num-calls max-calls) :max-calls-reached
        :else
        (let [kids (remove-previous-states
                    (make-children current-node) frontier (keys came-from))]
          (recur
           (add-children (pop frontier) (map (fn [kid] [kid (heuristic kid goal)]) kids )),
           (reduce (fn [cf child] (assoc cf child current-node)) came-from kids),
           (inc num-calls)
            )))))))
