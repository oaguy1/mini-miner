(local (window-width window-height) (love.window.getMode))
(local tile-width (math.floor (/ window-width 32)))
(local tile-height (math.floor (/ window-height 32)))
(local dirs {:left [-1 0] :right [1 0] :up [0 -1] :down [0 1]})
(local bg-tiles {"" 8 :lt 1 :t 2 :rt 3 :l 7 :f 8 :r 9 :lb 13 :b 14 :rb 15 :lr 20 :tb 22 :lrt 28 :lrb 29 :ltb 30 :rtb 31 :lrtb 32})
(local mined-tiles {"" 11 :lt 4 :t 5 :rt 6 :l 10 :f 11 :r 12 :lb 16 :b 17 :rb 18 :lr 25 :tb 24 :lrt 35 :lrb 36 :ltb 33 :rtb 34 :lrtb 37})
(var num-tiles (* tile-width tile-height))
(var end-map-gen-i 1)
(var end-map-gen-j 1)
(var value-multiplier 20)
(var base-x-noise (* 10000 (love.math.random)))
(var base-y-noise (* 10000 (love.math.random)))
(var level 1)

(fn register-item [img name desc effect]
  {:img img
   :name name
   :desc desc
   :cool-down 1
   :effect effect})

(fn new-particle [img collider x y dx dy ttl]
  {:img img
   :collider collider
   :x x
   :y y
   :dx dx
   :dy dy
   :ttl ttl})

(fn shallow-copy-table [tbl]
  (local new-tbl {})
  (each [k v (pairs tbl)]
    (tset new-tbl k v))
  new-tbl)

(fn new-rock [x y hp physics-world]
  (local collider  (physics-world:newCircleCollider x y 16))
  (collider:setType "static")
  (collider:setCollisionClass "rock")
  (local rock {:x x
               :y y
               :item nil
               :hp hp
               :collider collider})
  (rock.collider:setObject rock)
  rock)

(fn new-tile [x y]
  {:type :wall
   :tile-index 19
   :tile-key 0
   :mined false
   :value (love.math.noise (* x base-x-noise) (* y base-y-noise))})

(fn init-map []
  (let [tile-map []]
    (for [j 1 tile-height]
      (let [row []]
        (for [i 1 tile-width]
          (table.insert row (new-tile i j)))
        (table.insert tile-map row)))
    tile-map))

(fn random-item-from-table [tbl]
  (local keyset [])
  (each [key (pairs tbl)]
    (table.insert keyset key))

  (let [choice-index (love.math.random (length keyset))
        choice-key (. keyset choice-index)
        choice (. tbl choice-key)]
    (unpack choice)))

(fn random-dir []
  (random-item-from-table dirs))

(fn gen-map [max-tunnels max-length]
  (let [tile-map (init-map)
        start-tile (love.math.random num-tiles)]
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

    (set end-map-gen-i curr-i)
    (set end-map-gen-j curr-j)

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

  (for [j 0 (+ tile-height 1)]
    (for [i 0 (+ tile-width 1)]
      ;; handle borders
      (when (or (= i 0) (= i (+ 1 tile-width)) (= j 0) (= j (+ 1 tile-height)))
        (local block (physics-world:newRectangleCollider
                             (+ (* (- i 1)  32) 1)
                             (+ (* (- j 1) 32) 1)
                             30
                             30))
        (block:setType "static")
        (block:setCollisionClass "wall")
        (table.insert map-physics-objects block))

      ;; handle map
      (when (not (or (= i 0) (= i (+ 1 tile-width)) (= j 0) (= j (+ 1 tile-height))))
        (when (= (. (. (. tile-map j) i) :type) :wall)
            (local block (physics-world:newRectangleCollider
                                (+ (* (- i 1)  32) 1)
                                (+ (* (- j 1) 32) 1)
                                30
                                30))
            (block:setType :static)
            (block:setCollisionClass :wall)
            (table.insert map-physics-objects block)))))
  map-physics-objects)

(fn setup-rocks [tile-map physics-world threshold item-prototypes]
  (local rock-objects [])
  (for [j 1 (length tile-map)]
    (for [i 1 (length (. tile-map j))]
      (let [curr-tile (. (. tile-map j) i)
            mineral-val (love.math.random)]
        (when (and (not (= curr-tile.type :wall))
                   (> curr-tile.value threshold))
          (local rock (new-rock
                       (+ (* 32 (- i 1)) 16)
                       (+ (* 32 (- j 1)) 16)
                       3
                       physics-world))
          (when (> mineral-val (- .45 (/ level 100)))
            (tset rock :item (shallow-copy-table (. item-prototypes :copper))))
          (when (> mineral-val (- .75 (/ level 100)))
            (tset rock :item (shallow-copy-table (. item-prototypes :silver))))
          (when (> mineral-val (- .99 (/ level 100)))
            (tset rock :item (shallow-copy-table (. item-prototypes :gold))))

          (table.insert rock-objects rock)))))


  (let [random-idx (love.math.random (length rock-objects))
        random-rock (. rock-objects random-idx)]
    (tset random-rock :item (shallow-copy-table (. item-prototypes :new-level-ladder))))

  rock-objects)

(fn destroy-physics-objects [physics-objects]
  (each [_ obj (ipairs physics-objects)]
    (obj.fixture:destroy)))

(fn clear-remaining-objects [objects]
  (each [_ obj (ipairs objects)]
    (obj.collider:destroy)))

;; assumes obj has x and y properties
(fn find-object-index [obj objects]
  (var target-idx -1)
  (each [idx o (ipairs objects)]
    (when (and (= obj.x o.x) (= obj.y o.y))
      (set target-idx idx)))
  target-idx)

(fn destroy-rock [rock rock-objects physics-world small-rock-img particle-objects items]
  (let [rock-idx (find-object-index rock rock-objects)
        particle-force-multipier 5]
    (rock.collider:destroy)
    (when rock.item
      (tset rock.item :x rock.x)
      (tset rock.item :y rock.y)
      (local new-collider (physics-world:newCircleCollider rock.x rock.y 16))
      (new-collider:setCollisionClass :item)
      (new-collider:setType :static)
      (tset rock.item :collider new-collider)
      (new-collider:setObject rock.item)
      (table.insert items rock.item))

    (for [i 1 8]
      (local new-collider (physics-world:newCircleCollider rock.x rock.y 4))
      (new-collider:setCollisionClass :particle)
      (table.insert particle-objects (new-particle small-rock-img
                                                   new-collider
                                                   rock.x
                                                   rock.y
                                                   (* (love.math.random -1 1) particle-force-multipier)
                                                   (* (love.math.random -1 1) particle-force-multipier)
                                                   (love.math.random))))
    (table.remove rock-objects rock-idx)))

(fn use-item [item items]
  (item.collider:destroy)
  (let [item-idx (find-object-index item items)]
    (table.remove items item-idx))
  (item.effect))

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

  ;; physics init
  (global world (wf.newWorld 0 0 true))
  (world:addCollisionClassTable {:wall {}
                                 :rock {}
                                 :particle {}
                                 :item {:ignores [:weapon :player]}
                                 :player {:ignores [:weapon :item]}
                                 :weapon {:ignores [:player :item]}})

  
  (global level-map nil)
  (global level-map-physics-objects nil)
  (global level-map-rock-objects nil)
  (global tile-images (love.graphics.newImage "assets/Tiles.png"))
  (global tile-quads (gen-quad-table tile-images 32 32 1 37))
  (global particles [])
  (global item-prototypes {})
  (global item-objects [])
  (global generate-world (lambda []
                         (set base-x-noise (* 10000 (love.math.random)))
                         (set base-y-noise (* 10000 (love.math.random)))
                         (set level-map (gen-map 750 1))
                         (set level-map (auto-tile level-map))
                         (set level-map-physics-objects (setup-map-physics level-map world))
                         (set level-map-rock-objects (setup-rocks level-map world 0.9 item-prototypes))))


  (global hit-snd (love.audio.newSource "assets/hit.wav" :static))
  (global rock-break-snd (love.audio.newSource "assets/rock-break.wav" :static))
  (global new-level-snd (love.audio.newSource "assets/new-level.wav" :static))


  ;; global assets load
  (global rock-image (love.graphics.newImage "assets/rock.png"))
  (global small-rock-image (love.graphics.newImage "assets/small-rock.png"))

  ;; player collider and animation setup
  (global player {:collider (world:newCircleCollider
                         (+ (* end-map-gen-i 32) 16)
                         (+ (* end-map-gen-j 32) 16)
                         12)
                  :weapon-collider nil
                  :width 32
                  :height 32
                  :speed 100
                  :action-active false
                  :score 0
                  :animations {}})
  (player.collider:setCollisionClass :player)
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

  ;; items
  (local new-level-ladder-image (love.graphics.newImage "assets/new-level-ladder.png"))
  (tset item-prototypes :new-level-ladder (register-item new-level-ladder-image
                                                         "ladder"
                                                         "Brings you to the next level"
                                                         (lambda []
                                                           (set level (+ level 1))
                                                           (love.audio.play new-level-snd)
                                                           (destroy-physics-objects level-map-physics-objects)
                                                           (clear-remaining-objects level-map-rock-objects)
                                                           (clear-remaining-objects item-objects)
                                                           (set item-objects [])
                                                           (generate-world)
                                                           (player.collider:setPosition (+ (* end-map-gen-i 32) 16) (+ (* end-map-gen-j 32) 16)))))
  (local copper-image (love.graphics.newImage "assets/copper.png"))
  (tset item-prototypes :copper (register-item copper-image
                                               "copper"
                                               "Most common mineral"
                                               (lambda []
                                                 (set player.score (+ player.score 5)))))
  (local silver-image (love.graphics.newImage "assets/silver.png"))
  (tset item-prototypes :silver (register-item silver-image
                                               "silver"
                                               "Less common mineral"
                                               (lambda []
                                                 (set player.score (+ player.score 25)))))
  (local gold-image (love.graphics.newImage "assets/gold.png"))
  (tset item-prototypes :gold (register-item gold-image
                                               "gold"
                                               "Rare mineral"
                                               (lambda []
                                                 (set player.score (+ player.score 100)))))

  (generate-world)
  (player.collider:setPosition (+ (* end-map-gen-i 32) 16) (+ (* end-map-gen-j 32) 16))
  (global debug-strings []))


(fn love.update [dt]
  (world:update dt)
  (set debug-strings [])

  (when (love.keyboard.isDown :g)
    (destroy-physics-objects level-map-physics-objects)
    (clear-remaining-objects level-map-rock-objects)
    (clear-remaining-objects item-objects)
    (set item-objects [])
    (generate-world)
    (player.collider:setPosition (+ (* end-map-gen-i 32) 16) (+ (* end-map-gen-j 32) 16)))

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
        (player.weapon-collider:destroy)
        (tset player :weapon-collider nil)
        (tset (. (. player.animations) player.current-animation) :curr-frame next-frame)
        (tset player :current-animation player.previous-animation))
      (when (<= next-frame total-frames)
        (tset (. (. player.animations) player.current-animation) :curr-frame next-frame))))

  ;; enable action button
  (when (and (love.keyboard.isDown :space) (not player.action-active))
    (tset player :action-active true)
    (tset player :previous-animation player.current-animation)
    (when (= player.current-animation :running-down)
      (local collider (world:newCircleCollider
                       (player.collider:getX)
                       (+ (player.collider:getY) 5)
                       12))
      (collider:setCollisionClass :weapon)
      (tset player :weapon-collider collider)
      (tset player :current-animation :pickaxe-down))
    (when (= player.current-animation :running-up)
      (local collider (world:newCircleCollider
                       (player.collider:getX)
                       (- (player.collider:getY) 5)
                       12))
      (collider:setCollisionClass :weapon)
      (tset player :weapon-collider collider)
      (tset player :current-animation :pickaxe-up))
    (when (= player.current-animation :running-left)
      (local collider (world:newCircleCollider
                       (- (player.collider:getX) 5)
                       (player.collider:getY)
                       12))
      (collider:setCollisionClass :weapon)
      (tset player :weapon-collider collider)
      (tset player :current-animation :pickaxe-left))
    (when (= player.current-animation :running-right)
      (local collider (world:newCircleCollider
                       (+ (player.collider:getX) 5)
                       (player.collider:getY)
                       12))
      (collider:setCollisionClass :weapon)
      (tset player :weapon-collider collider)
      (tset player :current-animation :pickaxe-right)))

  ;; collision between rock and weapon
  (when (and player.weapon-collider (player.weapon-collider:enter :rock))
    (local collision-data (player.weapon-collider:getEnterCollisionData :rock))
    (local rock (collision-data.collider:getObject))
    (love.audio.stop rock-break-snd)
    (love.audio.play rock-break-snd)
    (destroy-rock rock level-map-rock-objects world small-rock-image particles item-objects))

  ;; collision between rock and item
  (when (and player.weapon-collider (player.weapon-collider:enter :item))
    (local collision-data (player.weapon-collider:getEnterCollisionData :item))
    (local item (collision-data.collider:getObject))
    (when (<= item.cool-down 0)
      (use-item item item-objects)))

  ;; update particles
  (each [idx particle (ipairs particles)]
    (when (< particle.ttl 0)
      (table.remove particles idx)
      (particle.collider:destroy))
    (when (> particle.ttl 0)
      (tset particle :ttl (- particle.ttl dt))
      (particle.collider:setLinearVelocity particle.dx particle.dy)))


  ;; update item cooldown
  (each [_ item (ipairs item-objects)]
    (when (> item.cool-down 0)
      (set item.cool-down (- item.cool-down dt))))


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

  (each [_ item (ipairs item-objects)]
    (love.graphics.draw item.img (- (item.collider:getX) 16) (- (item.collider:getY) 16)))

  (each [_ particle (ipairs particles)]
    (love.graphics.draw particle.img (particle.collider:getX) (particle.collider:getY)))

  (each [_ rock (ipairs level-map-rock-objects)]
    (love.graphics.draw rock-image (- rock.x 16) (- rock.y 16)))

  (love.graphics.setColor 1 1 1)
  (let [curr-player-anim (. (. player.animations) player.current-animation)]
    (love.graphics.draw curr-player-anim.img
                        (. curr-player-anim.quads
                           (math.floor curr-player-anim.curr-frame))
                        (- (player.collider:getX) 16)
                        (- (player.collider:getY) 16)))

  ;; really basic UI
  (love.graphics.print (.. "Level: " (tostring level)) 5 (* tile-height 32))
  (love.graphics.print (.. "Score: " (tostring player.score)) 125 (* tile-height 32))

  ;; debug info
  (each [idx str (ipairs debug-strings)]
    (love.graphics.print str 10 (* 15 idx))))
