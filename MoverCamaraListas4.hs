{-# LANGUAGE FlexibleContexts #-}
import Graphics.UI.GLUT
import Data.IORef
import GHC.Generics

_TIEMPO=0.1

data Cubos = NullC | Cubo {posX::Float, posY::Float, posZ::Float, velocidad::Float,sgnt::Cubos} deriving(Show, Eq)
data Balas = NullB | Bala {pX::Float, pY::Float, pZ::Float, vel::Float, sig::Balas} deriving(Show, Eq)
--Cubo posicionX posicionY posicionZ Velocidad Vivo Siguiente

main = do
    getArgsAndInitialize
    initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
    createWindow "OpenGL"
    depthFunc $= Just Less
    clearColor $= Color4 1 1 1 1
    windowSize $= Size 500 500
    windowPosition $= Position 300 100
    lighting $= Enabled
    position (Light 0) $= Vertex4 0 0 (-6) 1
    light (Light 0) $= Enabled
    let disparos = NullB
    tiempo <- newIORef 0.0
    angulo <- newIORef 0.0
    posicionx <- newIORef 0.0
    posiciony <- newIORef 0.0
    vposx <- newIORef 0.0
    vposy <- newIORef 0.0
    dispara <- newIORef NullB
    enemigos <- newIORef NullC
    displayCallback $= desplegar posicionx posiciony dispara enemigos
    idleCallback $= Just (temporizador tiempo posicionx posiciony vposx vposy dispara enemigos)
    keyboardMouseCallback $= Just (teclado vposx vposy dispara posicionx posiciony enemigos)
    mainLoop

desplegar posicionx posiciony dispara enemigos= do
    px <- get posicionx
    py <- get posiciony
    en <- get enemigos
    b <- get dispara
    if py > 1.3 then do 
         posiciony $= 1.3 
         py <- get posiciony
         posiciony $= 1.3 
    else if py < -1.3 then do 
         posiciony $= -1.3 
         py <- get posiciony
         posiciony $= -1.3 
    else
        posiciony $= py
    if px > 1.3 then do 
         posicionx $= 1.3 
         px <- get posicionx
         posicionx $= 1.3 
    else if px < -1.57 then do 
         posicionx $= -1.3 
         px <- get posicionx
         posicionx $= -1.3 
    else
        posicionx $= px
    loadIdentity
    matrixMode $= Modelview 0
    clear [ColorBuffer, DepthBuffer]
    --currentRasterPosition $= Vertex4 (-1.0) 0.9 (-0.20) 1.0
    --renderString TimesRoman24 $ "AnguloX: " ++ show(ax)
    --currentRasterPosition $= Vertex4 (-1.0) 0.5 (-0.20) 1.0
    --renderString TimesRoman24 $ "AnguloY: " ++ show(ay)
    --currentRasterPosition $= Vertex4 (-1.0) 0 (-0.20) 1.0
    --renderString TimesRoman24 $ "x: " ++ show(vx)
    --currentRasterPosition $= Vertex4 (-1.0) (-0.5) (-0.20) 1.0
    --renderString TimesRoman24 $ "y: " ++ show(vy)
    --currentRasterPosition $= Vertex4 (-1.0) (-0.8) (-0.20) 1.0
    --renderString TimesRoman24 $ "z: " ++ show(vz)
    matrixMode $= Projection
    perspective 60 1 1 5 
    --perspective fov aspect zNear zFar
    --lookAt (position camera) (lookAt camera) (upVector camera)
    lookAt (Vertex3 px py 0) (Vertex3 px py 5) (Vector3 0 1 0)
    flush
    preservingMatrix $ do
        --translate $ Vector3 (-0.25) (-0.25) (0::GLfloat)
        cubo 0.1 0 0 2
        dibujaCubos en
        dibujaBalas b
    swapBuffers

temporizador tiempo angulox anguloy vangulox vanguloy dispara enemigos= do
    ax <- get angulox
    ay <- get anguloy
    vx <- get vangulox
    vy <- get vanguloy
    t <- get tiempo
    bl <- get dispara
    en <- get enemigos
    if t>=1.0 then do
        tiempo $= 0.0
        angulox $= ax + vx
        anguloy $= ay + vy
        dispara $= actualizaBala bl
        enemigos $= actualizaCubo en
        postRedisplay Nothing
    else do
        dispara $= destruyeBalas bl
        enemigos $= destruyeCubo en
        tiempo $= t+_TIEMPO
        return ()

teclado vposx vposy dispara posicionx posiciony enemigos(Char 'a') Down _ _ = do
    vposx $= 0.05
    postRedisplay Nothing
teclado vposx vposy dispara posicionx posiciony enemigos(Char 'd') Down _ _ = do
    vposx $= -0.05
    postRedisplay Nothing
teclado vposx vposy dispara posicionx posiciony enemigos(Char 'w') Down _ _ = do
    vposy $= 0.05
    postRedisplay Nothing
teclado vposx vposy dispara posicionx posiciony enemigos(Char 's') Down _ _ = do
    vposy $= -0.05
    postRedisplay Nothing
teclado vposx vposy dispara posicionx posiciony enemigos(Char 'a') Up _ _ = do
    vposx $= 0
    postRedisplay Nothing
teclado vposx vposy dispara posicionx posiciony enemigos(Char 'd') Up _ _ = do
    vposx $= 0
    postRedisplay Nothing
teclado vposx vposy dispara posicionx posiciony enemigos(Char 'w') Up _ _ = do
    vposy $= 0
    postRedisplay Nothing
teclado vposx vposy dispara posicionx posiciony enemigos(Char 's') Up _ _ = do
    vposy $= 0
    postRedisplay Nothing
teclado vposx vposy dispara posicionx posiciony enemigos(Char 'j') Down _ _ = do
    e <- get dispara
    cb <- get enemigos
    x <- get posicionx
    y <- get posiciony
    let px = realToFrac (x)
    let py = realToFrac (y)
    dispara $= insertBala px py 1.0 0.05 e
    enemigos $= insertCubo 0 0 5 0.05 cb
    postRedisplay Nothing
teclado _ _ _ _ _ _ _ _ _ _= return ()

cubo tam px py pz = do
    materialDiffuse Front $= Color4 1 0 0 1
    superficie [(0+px,0+py,0+pz), (tam+px, 0+py, 0+pz), (tam+px, tam+py, 0+pz), (0+px, tam+py, 0+pz)] (0,0,-1)
    materialDiffuse Front $= Color4 0 1 0 1
    superficie [(0+px,0+py,0+pz), (0+px, tam+py, 0+pz), (0+px, tam+py, tam+pz), (0+px, 0+py, tam+pz)] (-1,0,0)
    materialDiffuse Front $= Color4 0 0 1 1
    superficie [(0+px, 0+py, tam+pz), (tam+px, 0+py, tam+pz), (tam+px, tam+py, tam+pz), (0+px, tam+py, tam+pz)] (0,0,1)
    materialDiffuse Front $= Color4 1 1 0 1
    superficie [(tam+px, 0+py, 0+pz), (tam+px, tam+py, 0+pz), (tam+px, tam+py, tam+pz), (tam+px, 0+py, tam+pz)] (1,0,0)
    materialDiffuse Front $= Color4 1 0 1 1
    superficie [(0+px, 0+py, 0+pz), (tam+px, 0+py, 0+pz), (tam+px, 0+py, tam+pz), (0+px, 0+py, tam+pz)] (0,-1,0)
    materialDiffuse Front $= Color4 0 1 1 1
    superficie [(0+px, tam+py, 0+pz), (tam+px, tam+py, 0+pz), (tam+px, tam+py, tam+pz), (0+px, tam+py, tam+pz)] (0,1,0)

puntero tam posx posy posz num = do
    let px = realToFrac (posx * num)
    let py = realToFrac (posy * num)
    let pz = realToFrac (posz * num)
    materialDiffuse Front $= Color4 0 0 0 1
    superficie [(px-tam,py-tam,pz), (px-tam, py+tam, pz), (px+tam, py+tam, pz), (px+tam, py -tam, pz)] (0,0,-1)

superficie :: [(GLfloat, GLfloat, GLfloat)] -> (GLfloat,GLfloat,GLfloat) -> IO ()
superficie vertices (nx,ny,nz) = do
    renderPrimitive Polygon $ do
        normal (Normal3 nx ny nz)
        mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z) vertices

insertBala :: Float -> Float -> Float -> Float -> Balas -> Balas
insertBala x y z v NullB = (Bala x y z v NullB)
insertBala x y z v (Bala posx posy posz velocidad sig) = Bala posx posy posz velocidad (insertBala x y z v sig)

actualizaBala :: Balas -> Balas
actualizaBala NullB = NullB
actualizaBala (Bala x y z v sig) = Bala x y (z + v) v (actualizaBala sig)

destruyeBalas :: Balas -> Balas
destruyeBalas NullB = NullB
destruyeBalas (Bala x y z v sig) = if z < 5 then Bala x y z v (destruyeBalas sig) else destruyeBalas sig

dibujaBalas NullB = postRedisplay Nothing
dibujaBalas (Bala posx posy posz v sig) = do
    let x = realToFrac posx
    let y = realToFrac posy
    let z = realToFrac posz
    materialDiffuse Front $= Color4 0 0 0 1
    superficie [(x-0.01,y-0.01,z), (x-0.01, y+0.01, z), (x+0.01, y+0.01, z), (x+0.01, y-0.01, z)] (0,0,-1)
    dibujaBalas sig

insertCubo :: Float -> Float -> Float -> Float -> Cubos ->Cubos
insertCubo x y z v NullC = (Cubo x y z v NullC)
insertCubo x y z v (Cubo posx posy posz velocidad sig) = Cubo posx posy posz velocidad (insertCubo x y z v sig)

actualizaCubo :: Cubos -> Cubos
actualizaCubo NullC = NullC
actualizaCubo (Cubo x y z v sig) = Cubo x y (z - v) v (actualizaCubo sig)

destruyeCubo :: Cubos -> Cubos
destruyeCubo NullC = NullC
destruyeCubo (Cubo x y z v sig) = if z > 0 then Cubo x y z v (destruyeCubo sig) else destruyeCubo sig

dibujaCubos NullC = postRedisplay Nothing
dibujaCubos (Cubo posx posy posz v sig) = do
    let x = realToFrac posx
    let y = realToFrac posy
    let z = realToFrac posz
    materialDiffuse Front $= Color4 0 0 1 1
    superficie [(x-0.1,y-0.1,z), (x-0.1, y+0.1, z), (x+0.1, y+0.1, z), (x+0.1, y-0.1, z)] (0,0,-1)
    dibujaCubos sig

