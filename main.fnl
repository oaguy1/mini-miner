(local (window-width window-height) (love.window.getMode))
(local tile-width (math.floor (/ window-width 32)))
(local tile-height (math.floor (/ window-height 32)))
(local dirs {:left [-1 0] :right [1 0] :up [0 -1] :down [0 1]})
(local bg-tiles {:lt 1 :t 2 :rt 3 :l 7 :f 8 :r 9 :lb 13 :b 14 :rb 15 :lr 20 :tb 22 :lrt 28 :lrb 29 :ltb 30 :rtb 31 :lrtb 32})
(var num-tiles (* tile-width tile-height))
(var start-tile 1)

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
  (let [tile-map (init-map)]
    (set start-tile (love.math.random num-tiles))
    (var curr-j (math.floor (/ start-tile tile-height)))
    (var curr-i (math.floor (math.fmod start-tile tile-height)))

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

(fn setup-map-physics [tile-map physics-world]
  (local map-physics-objects [])
  (for [j 1 (length tile-map)]
    (for [i 1 (length (. tile-map j))]
      (when (= (. (. tile-map j) i) 1)
        (local block (physics-world:newRectangleCollider
                             (+ (* (- i 1)  32) 1)
                             (+ (* (- j 1) 32) 1)
                             30
                             30))
        (block:setType "static")
        (table.insert map-physics-objects block))))
  map-physics-objects)

(fn destroy-physics-objects [physics-objects]
  (each [_ obj (ipairs physics-objects)]
    (: obj.fixture :destroy)))

(fn gen-quad-table [img tile-width tile-height first-tile num-tiles]
  (let [img-width (img:getWidth)
        img-height (img:getHeight)
        quads []]
    (for [i first-tile num-tiles]
      (table.insert quads (love.graphics.newQuad (+ (* i tile-width) i)
                                                 0
                                                 tile-width
                                                 tile-height
                                                 img-width
                                                 img-height)))
    quads))

(fn gen-animation-table [speed img quads]
  {:curr-frame 1 :speed speed :img img :quads quads})

(fn love.load []
  ;; start new thread listening on stdin
  (: (love.thread.newThread "require('love.event')
while 1 do love.event.push('stdin', io.read('*line')) end") :start)

  (global world (wf.newWorld 0 0 true))

  (global level-map (gen-map 750 1))
  (global level-map-tiles (auto-tile level-map))
  (global level-map-physics-objects (setup-map-physics level-map world))
  (global tile-images (love.graphics.newImage "assets/Tiles.png"))
  (global tile-quads (gen-quad-table tile-images 32 32 1 37))

  (local player-j (math.floor (/ start-tile tile-height)))
  (local player-i (math.floor (math.fmod start-tile tile-height)))
  (global player {:collider (world:newCircleCollider
                         (* (math.floor (/ start-tile tile-height)) 32)
                         (* (math.floor (math.fmod start-tile tile-height)) 32)
                         16)
                  :width 32
                  :height 32
                  :speed 100
                  :dx 0
                  :dy 0
                  :animations {}})
  (local player-front (love.graphics.newImage "assets/running-front.png"))
  (tset player :animations :running-down (gen-animation-table
                                          10
                                          player-front
                                          (gen-quad-table player-front 32 32 0 3)))
  (local player-back (love.graphics.newImage "assets/running-back.png"))
  (tset player :animations :running-up (gen-animation-table
                                        10
                                        player-back
                                        (gen-quad-table player-back 32 32 0 3)))
  (local player-left (love.graphics.newImage "assets/running-left.png"))
  (tset player :animations :running-left (gen-animation-table
                                        8
                                        player-left
                                        (gen-quad-table player-left 32 32 0 4)))
  (local player-right (love.graphics.newImage "assets/running.png"))
  (tset player :animations :running-right (gen-animation-table
                                           8
                                           player-right
                                           (gen-quad-table player-right 32 32 0 4)))
  (tset player :current-animation :running-down))

(fn love.handlers.stdin [line]
  ;; evaluate lines written to stdin as fennel
  (let [(ok val) (pcall fennel.eval line)]
    (print (if ok (fennel.view val) val))))


(fn love.update [dt]
  (world:update dt)

  (when (love.keyboard.isDown :g)
    (destroy-physics-objects level-map-physics-objects)
    (set level-map (gen-map 750 1))
    (set level-map-tiles (auto-tile level-map))
    (set level-map-physics-objects (setup-map-physics level-map world))
    (local player-j (math.floor (/ start-tile tile-height)))
    (local player-i (math.floor (math.fmod start-tile tile-height)))
    (player.collider:setPosition
       (* (math.floor (/ start-tile tile-height)) 32)
       (* (math.floor (math.fmod start-tile tile-height)) 32)))

  (var x-velocity 0)
  (var y-velocity 0)

  (each [key [dx dy] (pairs dirs)]
    (when (love.keyboard.isDown key)
      (let [curr-frame (. (. player.animations player.current-animation) :curr-frame)
            anim-speed (. (. player.animations player.current-animation) :speed)
            total-frames (length (. (. player.animations player.current-animation) :quads))]
        (set x-velocity (* dx player.speed))
        (set y-velocity (* dy player.speed))
        (var next-frame (+ (* anim-speed dt) curr-frame))
        (when (> next-frame total-frames)
          (set next-frame 1))
        (tset (. (. player.animations) player.current-animation) :curr-frame next-frame)
        (set player.current-animation (.. "running-" key)))))

  (player.collider:setLinearVelocity x-velocity y-velocity)

  ;; Quit when escape is pressed
  (when (love.keyboard.isDown :escape)
    (love.event.quit)))


(fn love.draw []
  (for [j 1 (length level-map-tiles)]
    (for [i 1 (length (. level-map-tiles j))]
      (love.graphics.draw tile-images
                          (. tile-quads (. (. level-map-tiles j) i))
                          (* (- i 1)  32)
                          (* (- j 1) 32))))


  (let [curr-player-anim (. (. player.animations) player.current-animation)]
    (love.graphics.draw curr-player-anim.img
                        (. curr-player-anim.quads
                           (math.floor curr-player-anim.curr-frame))
                        (- (player.collider:getX) 16)
                        (- (player.collider:getY) 16))))
