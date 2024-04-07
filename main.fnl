(local (window-width window-height) (love.window.getMode))
(local tile-width (math.floor (/ window-width 32)))
(local tile-height (math.floor (/ window-height 32)))
(local dirs {:left [-1 0] :right [1 0] :up [0 -1] :down [0 1]})
(local bg-tiles {"" 8 :lt 1 :t 2 :rt 3 :l 7 :f 8 :r 9 :lb 13 :b 14 :rb 15 :lr 20 :tb 22 :lrt 28 :lrb 29 :ltb 30 :rtb 31 :lrtb 32})
(local mined-tiles {"" 11 :lt 4 :t 5 :rt 6 :l 10 :f 11 :r 12 :lb 16 :b 17 :rb 18 :lr 25 :tb 24 :lrt 35 :lrb 36 :ltb 33 :rtb 34 :lrtb 37})
(var num-tiles (* tile-width tile-height))
(var start-tile 1)
(var value-multiplier 20)

(fn new-tile []
  {:type :wall
   :tile-index 19
   :tile-key 0
   :mined false
   :value (math.floor (* (love.math.random) value-multiplier))})

(fn init-map []
  (let [tile-map []]
    (for [j 1 tile-height]
      (let [row []]
        (for [i 1 tile-width]
          (table.insert row (new-tile)))
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
                 
          (tset (. (. tile-map curr-j) curr-i) :type :floor)
          (set curr-j (+ curr-j dj))
          (set curr-i (+ curr-i di)))))
    tile-map))

(fn auto-tile [tile-map]
  (for [j 1 (length tile-map)]
    (for [i 1 (length (. tile-map j))]
      (var key "")
      (let [curr-tile (. (. tile-map j) i)]
        (when (= curr-tile.type :floor)
          (when (or (= 1 i) (= :wall (. (. (. tile-map j) (- i 1)) :type)))
            (set key (.. key "l")))
          (when (or (= tile-width i) (= :wall (. (. (. tile-map j) (+ i 1)) :type)))
            (set key (.. key "r")))
          (when (or (= 1 j) (= :wall (. (. (. tile-map (- j 1)) i) :type)))
            (set key (.. key "t")))
          (when (or (= tile-height j) (= :wall (. (. (. tile-map (+ j 1)) i) :type)))
            (set key (.. key "b")))
        (tset curr-tile :tile-key key)
        (tset curr-tile :tile-index (. bg-tiles key))))))
  tile-map)

(fn setup-map-physics [tile-map physics-world]
  (local map-physics-objects [])
  (for [j 1 (length tile-map)]
    (for [i 1 (length (. tile-map j))]
      (when (= (. (. (. tile-map j) i) :type) :wall)
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
  (global level-map (auto-tile level-map))
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
                  :target-i player-i
                  :target-j player-j
                  :speed 100
                  :action-active false
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
  (local player-pickaxe-front (love.graphics.newImage "assets/pickaxe-front.png"))
  (tset player :animations :pickaxe-down (gen-animation-table
                                          12
                                          player-pickaxe-front
                                          (gen-quad-table player-pickaxe-front 32 32 0 4)))
  (local player-pickaxe-back (love.graphics.newImage "assets/pickaxe-back.png"))
  (tset player :animations :pickaxe-up (gen-animation-table
                                        12
                                        player-pickaxe-back
                                        (gen-quad-table player-pickaxe-back 32 32 0 4)))
  (local player-pickaxe-left (love.graphics.newImage "assets/pickaxe-left.png"))
  (tset player :animations :pickaxe-left (gen-animation-table
                                        12
                                        player-pickaxe-left
                                        (gen-quad-table player-pickaxe-left 32 32 0 4)))
  (local player-pickaxe-right (love.graphics.newImage "assets/pickaxe-right.png"))
  (tset player :animations :pickaxe-right (gen-animation-table
                                           12
                                           player-pickaxe-right
                                           (gen-quad-table player-pickaxe-right 32 32 0 4)))
  (tset player :current-animation :running-down)
  (tset player :previous-animation nil)

  (global debug-strings []))



(fn love.update [dt]
  (world:update dt)
  (set debug-strings [])

  (when (love.keyboard.isDown :g)
    (destroy-physics-objects level-map-physics-objects)
    (set level-map (gen-map 750 1))
    (set level-map (auto-tile level-map))
    (set level-map-physics-objects (setup-map-physics level-map world))
    (local player-j (math.floor (/ start-tile tile-height)))
    (local player-i (math.floor (math.fmod start-tile tile-height)))
    (player.collider:setPosition
       (* (math.floor (/ start-tile tile-height)) 32)
       (* (math.floor (math.fmod start-tile tile-height)) 32)))

  (var x-velocity 0)
  (var y-velocity 0)

  (each [key [dx dy] (pairs dirs)]
    (when (and (love.keyboard.isDown key) (not player.action-active))
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

  ;; update action animation and toggle
  (when player.action-active
    (let [curr-frame (. (. player.animations player.current-animation) :curr-frame)
          anim-speed (. (. player.animations player.current-animation) :speed)
          total-frames (length (. (. player.animations player.current-animation) :quads))]
      (var next-frame (+ (* anim-speed dt) curr-frame))
      (when (> next-frame total-frames)
        (set next-frame 1)
        (tset player :action-active false)
        (tset (. (. player.animations) player.current-animation) :curr-frame next-frame)
        (tset player :current-animation player.previous-animation))
      (when (<= next-frame total-frames)
        (tset (. (. player.animations) player.current-animation) :curr-frame next-frame))))

  ;; update player's target tile
  (let [player-x (player.collider:getX)
        player-y (player.collider:getY)
        player-i (+ (math.floor (/ player-x 32)) 1)
        player-j (+ (math.floor (/ player-y 32)) 1)
        mod-i (math.fmod player-x 32)
        mod-j (math.fmod player-y 32)]
    (table.insert debug-strings (.. "player x: " (tostring player-x)))
    (table.insert debug-strings (.. "player y: " (tostring player-y)))
    (table.insert debug-strings (.. "player i: " (tostring player-i)))
    (table.insert debug-strings (.. "player j: " (tostring player-j)))
    (table.insert debug-strings (.. "player mod i: " (tostring mod-i)))
    (table.insert debug-strings (.. "player mod j: " (tostring mod-j)))
    (when (= player.current-animation :running-down)
      (if (and (> mod-j 8)
               (< player-j tile-height)
               (= :floor (. (. (. level-map (+ player-j 1)) player-i) :type)))
          (tset player :target-j (+ player-j 1))
          (tset player :target-j player-j))
      (tset player :target-i player-i))
    (when (= player.current-animation :running-up)
      (if (and (< mod-j 24)
               (> player-j 1)
               (= :floor (. (. (. level-map (- player-j 1)) player-i) :type)))
          (tset player :target-j (- player-j 1))
          (tset player :target-j player-j))
      (tset player :target-i player-i))
    (when (= player.current-animation :running-left)
      (if (and (< mod-i 24)
               (> player-i 1)
               (= :floor (. (. (. level-map player-j) (- player-i 1)) :type)))
          (tset player :target-i (- player-i 1))
          (tset player :target-i player-i))
      (tset player :target-j player-j))
    (when (= player.current-animation :running-right)
      (if (and (> mod-i 8)
               (< player-i tile-width)
               (= :floor (. (. (. level-map player-j) (+ player-i 1)) :type)))
          (tset player :target-i (+ player-i 1))
          (tset player :target-i player-i))
      (tset player :target-j player-j)))


  ;; enable action button
  (when (and (love.keyboard.isDown :space) (not player.action-active))
    (tset player :action-active true)
    (tset player :previous-animation player.current-animation)
    (when (= player.current-animation :running-down)
      (tset player :current-animation :pickaxe-down))
    (when (= player.current-animation :running-up)
      (tset player :current-animation :pickaxe-up))
    (when (= player.current-animation :running-left)
      (tset player :current-animation :pickaxe-left))
    (when (= player.current-animation :running-right)
      (tset player :current-animation :pickaxe-right))

    (let [target-tile (. (. level-map player.target-j) player.target-i)]
      (when (not target-tile.mined)
        (tset target-tile :mined true)
        (tset target-tile :tile-index (. mined-tiles target-tile.tile-key)))))

  ;; Quit when escape is pressed
  (when (love.keyboard.isDown :escape)
    (love.event.quit)))


(fn love.draw []
  (for [j 1 (length level-map)]
    (for [i 1 (length (. level-map j))]
      (let [curr-tile (. (. level-map j) i)]
        (love.graphics.draw tile-images
                            (. tile-quads curr-tile.tile-index)
                            (* (- i 1)  32)
                            (* (- j 1) 32)))))


  (let [curr-player-anim (. (. player.animations) player.current-animation)]
    (love.graphics.draw curr-player-anim.img
                        (. curr-player-anim.quads
                           (math.floor curr-player-anim.curr-frame))
                        (- (player.collider:getX) 16)
                        (- (player.collider:getY) 16)))

  ;; debug info
  (each [idx str (ipairs debug-strings)]
    (love.graphics.print str 10 (* 15 idx)))
  (love.graphics.rectangle "line" (* 32 (- player.target-i 1)) (* 32 (- player.target-j 1)) 32 32)
  )
