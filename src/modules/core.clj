(ns modules.core
  (:use [clojure.set]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn base-method 
  ([f]
   (base-method :override f))
  ([merge-type f]
   (with-meta f {:merge-type merge-type})))

;; base data ;;;;;;;;;;;;;;;;;;;;;;;;;;
(def base-invention (atom {:inventors []
                     :methods {:function-x (base-method :stack (fn [self]
                                                                 (println "base invention")))}}))


;; client data ;;;;;;;;;;;;;;;;;;;;;;;;
(def acme-invention {:has-explosives true
                     :methods {:ignite (fn [{:keys [has-explosives]}]
                                          (if has-explosives
                                            "!!!BOOM!!!"
                                            "~click~"))}})

(def dunder-mifflin-invention {:paper-used 0
                               :methods {:function-x (fn [self]
                                                       (println "dunder mifflin invention"))
                                         :paper-used (fn [{:keys [paper-used]}]
                                                        (str "this invention used " paper-used " sheets of paper"))}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Framework ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; method merge helpers
(defn common-keys [map-1 map-2]
  (intersection (set (keys map-1))
                (set (keys map-2))))

(defn common-methods [o1 o2]
  (common-keys (:methods o1) (:methods o2)))

;; merging methods in different ways using multimethod
(defmulti merge-methods 
  (fn [base-fn _]
    (:merge-type (meta base-fn))))

(defmethod merge-methods :override [base-fn client-fn]
  client-fn)

(defmethod merge-methods :stack [base-fn client-fn]
  (fn [& args]
    (apply base-fn args)
    (apply client-fn args)))
;; end of multimethod ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; returns a new map that contains the merged method
(defn merge-method [m method-key base client]
  (merge m {method-key (merge-methods (method-key (:methods base))
                                      (method-key (:methods client)))}))

;; merges all of the common methods on base and client
(defn reverse-merge-methods [base client]
  (merge base {:methods (reduce #(merge-method %1 %2 base client) (:methods base) (common-methods base client))}))

(defn build-invention 
  ([client]
   (build-invention client {}))
  ([client starting-values]
   (reset! base-invention (reverse-merge-methods @base-invention client))
   (merge @base-invention client starting-values)))

(defn update-invention [invention updated-fields]
   (merge invention updated-fields))

(defn method [invention method-name & method-args]
   (apply ((:methods invention) method-name) invention method-args))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; application ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(println)
(println "original base invention function-x call:")
(method @base-invention :function-x)
(println)

(def acme (build-invention acme-invention {:inventors ["Road Runner"]
                                           :has-explosives false} ))

(def dunder-mifflin (build-invention dunder-mifflin-invention {:inventors ["John" "Ringo" "Paul"]
                                                               :paper-used 3}))
(println)
(println "base invention function-x call after createing clients:")
(method @base-invention :function-x)
(println)

(println)
(println "objects:")
(prn acme)
(prn dunder-mifflin)
(println)
(println "method calls results:")
(prn (method acme :ignite))
(prn (method dunder-mifflin :paper-used))
