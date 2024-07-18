(ns evidencia3.core)
(require '[clojure.edn :as edn])

; ---
; Funciones Miscelaneas
; ---

; ---
; 1. Estadisticas Propias
; ---

; --- Cantidad Total de Vehiculos Cruzados ---
; Cantiad total por semaforo
(defn veh-sem [coches]
  (if (empty? coches)
    0
    (+ 1 (veh-sem (rest coches)))
  )  
)

; Funcion prepradora para total por semaforo
(defn prep-veh-sem [coches]
  (map
    (fn [i]
      (veh-sem (second i))  
    )
   
    coches
  )
)

; Cantiad total por curce
(defn veh-crucero [coches]
  (apply + (prep-veh-sem coches))  
)

; --- Promedio de Tiempo de Cruce ---
; Crea una lista con los tiempos en verde de los semaforos
(defn verdes [coches config]
  (cond
    (empty? coches) '()
    (empty? config) (cons 30 (verdes (rest coches) config)) ; En caso de que no se defina un tiempo en config
    (= (first (first coches)) (first (first config))) (cons (second (first config)) (verdes (rest coches) (rest config)))
    (not (= (first (first coches)) (first (first config)))) (cons 30 (verdes (rest coches) config))
  )  
)

; Funcion que determina cuanto tiempo falta para que se prenda un semaforo despues de una iteracion completa del crucero
(defn t-rest [idx verdes]
  (if (= (- idx 1) 0)
    0
    (+ (first verdes) (t-rest (- idx 1) (rest verdes)))
  )  
)

; Tiempo total de Crucero
(defn tim-tot-cru [coches config]
  (apply + (verdes coches config))  
)

; Funcion de Promedios
(defn prom [lst]
  (/ (apply + lst) (count lst))
)

; Funcion para determinar tiempo restante
(defn rem-t [x t]
  (if (= x 0)
    0
    (- x (* (quot x t) t))
  )  
)

; Determina cuanto tiempo le tomo a un coche esperar
;t = tiempo total del crucer
;r = tiempo del semaforo en rojo despues de una iteracion
;c = constante de adicion
(defn pasa-sem [coches t r c] 
  (if (empty? coches)
    '()
    (cond
      (= (first (rest (first coches))) 2) (cons c (pasa-sem (rest coches) t r c)) ; Si el coche esta en el segundo carril pasa sin esperar
      (>= (rem-t (first (first coches)) t) r) (cons (+ c (rem-t (first (first coches)) t)) (pasa-sem (rest coches) t r c)) ; Si el coche llega y hay luz verde pasa si esperar
      :else (cons (+ c (- r (rem-t (first (first coches)) t))) (pasa-sem (rest coches) t r c)) ; Si el coche llega con luz verde se calcula cuanto tiene que esperar
    )
  )
)

; Promedio de espera por Semaforo
(defn prom-sem [coches config]
  (pmap
    (fn [i]
      (prom (pasa-sem (second i) (tim-tot-cru coches (second config)) (t-rest (first i) (verdes coches (second config))) (first config)))
    )
   
    coches
  )
)

; Promedio de espera por crucero
(defn prom-crucero [coches config]
  (prom (prom-sem coches config))
)

; --- Tiempo en Verde sin Coches ---
; Regresa el tiempo en verde de un semaforo
(defn tiempo-verde [lst idx]
  (if (= idx 0)
    0
    (if (= idx 1)
      (first lst)
      (+ (first lst) (tiempo-verde (rest lst) (- idx 1)))
    )
  )  
)

; Recorre la fila de los coches
; first tiempo = min
; rest  tiempo = max
(defn recorrer-fila [coches tiempo ttotal smax]
  (cond
    (empty? coches) 0
    (and (>= (ffirst coches) (second tiempo)) smax)
    (+ 1 (recorrer-fila coches [(+ ttotal (first tiempo)) (+ ttotal (second tiempo))] ttotal true))
    
    (>= (ffirst coches) (second tiempo))
      (+ 0 (recorrer-fila coches [(+ ttotal (first tiempo)) (+ ttotal (second tiempo))] ttotal true))
    
    (<= (ffirst coches) (first tiempo))
      (+ 0 (recorrer-fila (rest coches) tiempo ttotal false))
    
    :else 0
  )
)

; Regresa el tiempo muerto de un semaforo
(defn tiempo-muerto [coches config]
  (pmap
    (fn [i]
      (recorrer-fila (second i)
        (list 
          (tiempo-verde (verdes coches (second config)) (- (first i) 1))
          (tiempo-verde (verdes coches (second config)) (first i))
        )
        (tim-tot-cru coches (second config))
        false
      )
    )
   
    coches
  )
)

; ---
; 2. Lista Clasificada por Cantidad de Semaforos
; ---

; Regresa una lista con el nombre y cantidad de semaforos de un crucero
(defn cant-sem [nombre coches]
  (list nombre (count coches))  
)

; Ordena la lista de acuerdo a la cantidad de semaforos
(defn sem-quick-sort [lst]
  (if (empty? lst)
    '()
    (let [pivot (second (first lst))
          rest-lst (rest lst)
          smaller (filter #(<= (second %) pivot) rest-lst)
          larger (filter #(> (second %) pivot) rest-lst)]
     
      (concat (sem-quick-sort smaller) (list (first lst)) (sem-quick-sort larger))
    )
  )
)

; Hace un group-by para juntar todos los cruceros de acuerdo a su semaforo
(defn group-cru [lst]
  (let [grouped (group-by second lst)]
    (map (fn [[x y]] (cons x (map first y))) grouped) ; ME TOMO 3 HRS CONSEGUIR QUE JALARA ESTA PARTE AAAAAAAAA
  )
)

; ---
; 3. Tiempo Promedio de Espera Total
; ---

(defn prom-tot [coll]
  (prom coll)
)

; ---
; 4. 10% Mayor Tiempo de Espera por Crucero
; ---

; Le pone el nombre del crucero al promedio
(defn cru-namer [i nombre]
  (list i nombre)  
)

; Organiza los datos de mayor a menor
(defn cru-quick-sort [lst]
  (if (empty? lst)
    '()
    (let [pivot (first (first lst))
          rest-lst (rest lst)
          smaller (filter #(<= (first %) pivot) rest-lst)
          larger (filter #(> (first %) pivot) rest-lst)]
    
      (concat (cru-quick-sort larger) (list (first lst)) (cru-quick-sort smaller))
    )
  )
)

; Agarra el 10% de los datos
(defn top-10-cru [lst]
  (take (quot (count lst) 10) lst)
)

; ---
; 5. 10% Mayor Semaforo Verde sin Coches
; ---

; Ordena los semaforos de acuerdo a cuantos semaforos verde sin coches tuvo
(defn ver-quick-sort [lst]
  (if (empty? lst)
    '()
    (let [pivot (first (first lst))
          rest-lst (rest lst)
          smaller (filter #(<= (first %) pivot) rest-lst)
          larger (filter #(> (first %) pivot) rest-lst)]
     
      (concat (ver-quick-sort larger) (list (first lst)) (ver-quick-sort smaller))
    )
  )
)

; Regresa los tiempos de los semaforos en una lista con el nombre de su crucero su indice y su cantidad de verdes vacios
; Formato de celula:
; (veces-vacio nom-crucero idx-semaforo)
(defn tiempos-idx [lst nombre]
  (map-indexed (fn [idx val] (list val nombre (inc idx))) lst)  
)

(defn top-10-sem [lst]
  (take (quot (count lst) 10) lst)  
)

; ---
; main
; ---

; Lector de archivos
(defn read-file-as-list [file-path]
  (let [content (slurp file-path)]
    (edn/read-string content)
  )
)

; 1. Estadisticas de Archivos particulares
(defn estad-part []
  (let [coches-nombre (read-line)
        config-nombre (read-line)
        coches (first (list (read-file-as-list (str "src/evidencia3/" coches-nombre))))
        config (first (list (read-file-as-list (str "src/evidencia3/" config-nombre))))]
  
    (println "---" coches-nombre " y " config-nombre "---")
  
    (println "Promedio Tiempo Cruce de Crucero:")
    (println (prom-crucero coches config))
  
    (println "Promedio Tiempo Cruce de Semaforo:")
    (println (prom-sem coches config))
  
    (println "Vehiculos Totales por Cruce:")
    (println (veh-crucero coches))
  
    (println "Vehiculos Totales por Semaforo:")
    (println (prep-veh-sem coches))
  
    (println "Tiempo en Verde sin Coches:")
    (println (tiempo-muerto coches config))
    
    (cond
      (= 0 (compare (read-line) "y")) (estad-part)
      :else nil
    )
  ) 
)

; 2. Llamado a Clasificacion de Semaforos
(defn clas-sem [masterconfig]
  (group-cru (sem-quick-sort 
    (pmap
      (fn [i]
        (let [nombre (nth i 0)
              coches (first (list (read-file-as-list (str "src/evidencia3/" (nth i 1)))))]
         
          (cant-sem nombre coches)
        )
      )
     
      masterconfig
    )
  ))
)

; 3. Llamado a Mapeo de Promedio de Cruceros
(defn prom-esp-call [masterconfig]
  (prom-tot
    (pmap
      (fn [i]
        (let [coches (first (list (read-file-as-list (str "src/evidencia3/" (nth i 1)))))
              config (first (list (read-file-as-list (str "src/evidencia3/" (nth i 2)))))]
          
          (prom-crucero coches config)
        ) 
      )
    
       masterconfig
    )
  )
)

; 4. Llama el top 10% de Cruceros con mas tiempo de espera
(defn top-prom-esp [masterconfig]
  (top-10-cru 
    (cru-quick-sort 
      (pmap
        (fn [i]
          (let [nombre (nth i 0)
                coches (first (list (read-file-as-list (str "src/evidencia3/" (nth i 1)))))
                config (first (list (read-file-as-list (str "src/evidencia3/" (nth i 2)))))]
          
            (cru-namer (prom-crucero coches config) nombre)
          )
        )
        
        masterconfig
      )
    )
  )
)

; 5. Llama el top 10% de tiempo en verde
(defn top-verde-vacio [masterconfig]
  (top-10-sem 
    (ver-quick-sort 
      (apply concat 
        (pmap
          (fn [i]
            (let [nombre (nth i 0)
                  coches (first (list (read-file-as-list (str "src/evidencia3/" (nth i 1)))))
                  config (first (list (read-file-as-list (str "src/evidencia3/" (nth i 2)))))]
             
              (tiempos-idx (tiempo-muerto coches config) nombre)
            )
          )
        
          masterconfig
        )
      )
    )
  )
)

(defn main []
  (println "Escribe el nombre de tu archivo masterconfig:")
  (let [masterconfig (read-file-as-list (str "src/evidencia3/" (read-line)))]
    (println "Input 1: Coches.txt")
    (println "Input 2: Config.txt")
    (println "Input 3: Continuar con estadisticas particulares (y/n)")
    (println "=== Estadisticas Particulares ===")
    (estad-part)
    
    (println "=== Estadisticas de Simulacion ===")
  
    (println "Cruceros Clasificados por Cantiad de Semaforos:")
    (println (clas-sem masterconfig))
    
    (println "Promedio Total de Espera en Simulacion: ")
    (println (prom-esp-call (first (list masterconfig))))
  
    (println "Top 10% de Cruceros con mas espera:")
    (println (top-prom-esp masterconfig))
  
    (println "Top 10% de Semaforos en Verde sin Coches en el Crucero:")
    (println (top-verde-vacio masterconfig))
  )

  "Hijole que bien hecho esta este proyecto :P"
)

(main)