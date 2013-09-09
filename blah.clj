(def rob { :tech [ "C#", "JS", "Clojure", "Node", "MVC"]
           :location "everywhere"
           :activities (fn [need]
                         (match need
                           "hungry" (eat pizza) 
                           "thirsty" (drink vodka)
                           "poor" (code csharp)
                           "curious" (code anything)
                           "bored" (code games)
                           :else (sleep)))})

