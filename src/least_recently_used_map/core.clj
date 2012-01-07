(ns least-recently-used-map.core
  (:require [clojure.data.priority-map :as p-map]
            [clj-time.core :as time])
  (:import clojure.lang.MapEntry))

(declare lrumap-empty)

(deftype PersistentLRUMap [priority-map max-size removal-fn removal-state index _meta]
  Object
  (toString [this] (str (doall (.seq this))))

  clojure.lang.ILookup
  (valAt [this item] (get-in priority-map [item :data]))
  (valAt [this item not-found] (get-in priority-map [item :data] not-found))

  clojure.lang.IPersistentMap
  (count [this] (count priority-map))

  (assoc [this item value]
    (if (contains? priority-map item)
      (PersistentLRUMap. (assoc-in priority-map [item :time] index)
                         max-size removal-fn removal-state (inc index) _meta)
      (if (< (count priority-map) max-size)
        (PersistentLRUMap. (assoc priority-map item {:time index :data value})
                           max-size removal-fn removal-state (inc index) _meta)
        (let [[k val-map] (peek priority-map)
              new-priority-map (pop priority-map)
              new-state (removal-fn removal-state (MapEntry. k (:data val-map)))]
          (PersistentLRUMap. (assoc new-priority-map item {:time index :data value})
                             max-size removal-fn new-state (inc index) _meta)))))

  (empty [this] (lrumap-empty max-size))

  (cons [this [item value]] (.assoc this item value))

  (equiv [this o] (.equiv priority-map o))
  (hashCode [this] (.hashCode priority-map))
  (equals [this o] (.equals priority-map o))

  (containsKey [this item] (contains? priority-map item))

  (entryAt [this k]
    (let [v (.valAt this k this)]
      (when-not (identical? v this)
        (MapEntry. k (:data v)))))

  (seq [this]
    (concat (for [[k vm] priority-map]
              (MapEntry. k (:data vm)))
            (when removal-state
             (list (MapEntry. :state removal-state)))))

  ;; Maybe `without` should call `removal-fn`, but that seems more for
  ;; when an element is removed because it's too old.
  (without [this item]
    (PersistentLRUMap. (dissoc priority-map item) max-size removal-fn removal-state index _meta))

  clojure.lang.IPersistentStack
  (peek [this]
    (when-let [[k vm] (peek priority-map)]
      (MapEntry. k (:data vm))))

  ;; Again, this could call `removal-fn`, but it seems wrong to me.
  (pop [this]
    (PersistentLRUMap. (pop priority-map) max-size removal-fn removal-state index _meta))

  clojure.lang.IFn
  (invoke [this k] (.valAt this k))
  (invoke [this k not-found] (.valAt this k not-found))

  clojure.lang.IObj
  (meta [this] _meta)
  (withMeta [this m]
    (PersistentLRUMap. priority-map max-size removal-fn removal-state index m)))

(defn- lru-comparator
  ([a] 0)
  ([a b] ((comparator <) (:time a) (:time b))))

(defn- discard
  [x state])

(defn- lrumap-empty
  [max-value]
  (PersistentLRUMap. (p-map/priority-map-by lru-comparator) max-value discard nil 0 {}))
(defn- lrumap-empty-with
  [max-value removal-fn initial-removal-state]
  (PersistentLRUMap. (p-map/priority-map-by lru-comparator) max-value removal-fn initial-removal-state 0 {}))

(defn lru-map
  [max-value & keyvals]
  (reduce conj (lrumap-empty max-value) (partition 2 keyvals)))

(defn lru-map-with
  [max-value removal-function initial-removal-state & keyvals]
  (reduce conj (lrumap-empty-with max-value removal-function initial-removal-state)
          (partition 2 keyvals)))

(comment
  (def m (apply lru-map-with 4 conj [] (range 20)))
  (def m (apply lru-map 4 (range 20)))
  (str m)
  (dissoc (into {} m) :state)
  (vals m)
 )