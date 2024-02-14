(local (window-width window-height) (love.window.getMode))
(local tile-width (math.floor (/ window-width 32)))
(local tile-height (math.floor (/ window-height 32)))
(local dirs {:up [-1 0] :down [1 0] :left [0 -1] :right [0 1]})
(local bg-tiles {:lt 1 :t 2 :rt 3 :l 7 :f 8 :r 9 :lb 13 :b 14 :rb 15 :lr 20 :tb 22 :lrt 28 :lrb 29 :ltb 30 :rtb 31 :lrtb 32})

(fn init-map []
  (let [tile-map []]
    (for [j 1 tile-height]
      (let [row []]
        (for [i 1 tile-width]
          (table.insert row 1))
        (table.insert tile-map row)))
    tile-map))

(fn random-dir []
  (local keyset [])
  (each [key (pairs dirs)]
    (table.insert keyset key))

  (let [choice-index (love.math.random (length keyset))
        choice-key (. keyset choice-index)
        choice (. dirs choice-key)]
    (unpack choice)))

(fn print-tile-map [tile-map]
  (for [j 1 (length tile-map)]
    (for [i 1 (length (. tile-map j))]
      (io.write (. (. tile-map j) i)))
    (io.write  "\n")))

(fn gen-map [max-tunnels max-length]
  (let [num-tiles (* tile-width tile-height)
        start (love.math.random num-tiles)
        tile-map (init-map)]
    (var curr-j (math.floor (/ start tile-height)))
    (var curr-i (math.floor (math.fmod start tile-height)))

    (for [tunnel 1 max-tunnels]
      (let [(dj di) (random-dir)]
        (for [tunnel-idx 1 max-length]
          (when (< curr-j 1)
            (set curr-j 1))
          (when (> curr-j tile-height)
            (set curr-j tile-height))
          (when (< curr-i 1)
            (set curr-i 1))
          (when (> curr-i tile-width)
            (set curr-i tile-width))
                 
          (tset tile-map curr-j curr-i 0)
          (set curr-j (+ curr-j dj))
          (set curr-i (+ curr-i di)))))
    tile-map))

(fn auto-tile [tile-map]
  (local new-map (init-map))
  (for [j 1 (length tile-map)]
    (for [i 1 (length (. tile-map j))]
      (var key "")
      (when (= (. (. tile-map j) i) 0)
        (when (or (= 1 i) (= 1 (. (. tile-map j) (- i 1))))
          (set key (.. key "l")))
        (when (or (= tile-width i) (= 1 (. (. tile-map j) (+ i 1))))
          (set key (.. key "r")))
        (when (or (= 1 j) (= 1 (. (. tile-map (- j 1)) i)))
          (set key (.. key "t")))
        (when (or (= tile-height j) (= 1 (. (. tile-map (+ j 1)) i)))
          (set key (.. key "b")))
        (if (= key "")
            (tset new-map j i 8) ;; center tile
            (tset new-map j i (. bg-tiles key))))
      (when (= (. (. tile-map j) i) 1)
        (tset new-map j i 19)))) ;; blank tile
  new-map)
          

(fn love.load []
  ;; start new thread listening on stdin
  (: (love.thread.newThread "require('love.event')
while 1 do love.event.push('stdin', io.read('*line')) end") :start)

  (global level-map (gen-map 750 1))
  (global level-map-tiles (auto-tile level-map))
  (global tile-images (love.graphics.newImage "assets/Tiles.png"))
  (global tile-quads [])
  (let [img-width (tile-images:getWidth)
        img-height (tile-images:getHeight)
        tile-width 32
        tile-height 32]
    (for [i 1 37]
      (table.insert tile-quads (love.graphics.newQuad (+ (* i tile-width) i)
                                                      0
                                                      tile-width
                                                      tile-height
                                                      img-width
                                                      img-height)))))

(fn love.handlers.stdin [line]
  ;; evaluate lines written to stdin as fennel
  (let [(ok val) (pcall fennel.eval line)]
    (print (if ok (fennel.view val) val))))


(fn love.update [dt]

  ;; Quit when escape is pressed
  (when (love.keyboard.isDown :g)
    (set level-map (gen-map 750 1))
    (set level-map-tiles (auto-tile level-map)))

  ;; Quit when escape is pressed
  (when (love.keyboard.isDown :escape)
    (love.event.quit)))


(fn love.draw []
  (for [j 1 (length level-map-tiles)]
    (for [i 1 (length (. level-map-tiles j))]
      (love.graphics.draw tile-images
                          (. tile-quads (. (. level-map-tiles j) i))
                          (* (- i 1)  32)
                          (* (- j 1) 32)))))
