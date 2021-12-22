(ns spimp-lab2.core
    (:require [clojure.core.async :refer [>! <! <!! >!! timeout chan alts! go go-loop close!]]))

(defn channel-to-print [channel]
    (<!! (go-loop []
             (when-let [x (<! channel)]
                 (println x)
                 (recur)))))

(defn vec-to-channel [vec]
    (let [channel (chan 10)]
        (go
            (doseq [x vec]
                (>! channel x))
            (close! channel))
        channel))

(defn channels-take [channels]
    (loop [i 0 e nil]
        (if (< i (count channels))
            (if-let [x (<!! (nth channels i))]
                (if (nil? e)
                    (recur (inc i) x)
                    (if (= x e)
                        (recur (inc i) e)
                        (do
                            (loop [j (inc i)]
                                (when (< j (count channels))
                                    (<!! (nth channels j))
                                    (recur (inc j))))
                            [nil true])))
                [nil false])
            [e true])))

(defn prediction-writer [channels output]
    (go-loop []
        (let [[v c] (channels-take channels)]
            (if c
                (do
                    (when (not (nil? v))
                        (>! output v))
                    (recur))
                (close! output)))))

(defn -main [& _]
    ;; Написать функцию принимающую канал для записи и набор каналов для чтения,
    ;; и в случае если из всех каналов из набора
    ;; пришло одно и то же сообщение то записывать его в канал для записи
    (let [channels [(vec-to-channel [4 1 0 8 0 7 3])
                    (vec-to-channel [4 1 2 7 0 7 2])
                    (vec-to-channel [2 1 2 6 0 7 3])]
          output (chan 10)]
        (prediction-writer channels output)
        (channel-to-print output)))
