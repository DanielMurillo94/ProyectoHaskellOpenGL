{-# LANGUAGE FlexibleContexts #-}
import Graphics.UI.GLUT
import Data.IORef
import GHC.Generics
import System.Random
import System.IO.Unsafe

_TIEMPO=0.1

data Cubos = NullC | Cubo {posX::Float, posY::Float, posZ::Float, velocidad::Float,sgnt::Cubos} deriving(Show, Eq)
data Balas = NullB | Bala {pX::Float, pY::Float, pZ::Float, vel::Float, sig::Balas} deriving(Show, Eq)
--Cubo posicionX posicionY posicionZ Velocidad Vivo Siguiente

main = do
    getArgsAndInitialize
    initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
    createWindow "Disparos Locos"
    depthFunc $= Just Less
    clearColor $= Color4 1 1 1 1
    windowSize $= Size 500 500
    windowPosition $= Position 300 100
    lighting $= Enabled
    position (Light 0) $= Vertex4 0 0 (1) 1
    light (Light 0) $= Enabled
    position (Light 1) $= Vertex4 0 0 (4) 1
    light (Light 1) $= Enabled
    let disparos = NullB
    tiempo <- newIORef 0.0
    angulo <- newIORef 0.0
    posicionx <- newIORef 0.0
    posiciony <- newIORef 0.0
    vposx <- newIORef 0.0
    vposy <- newIORef 0.0
    dispara <- newIORef NullB
    enemigos <- newIORef NullC
    randind <- newIORef 0
    frecenem <- newIORef 0
    dfrecenem <- newIORef 0.001
    puntajeM <- newIORef 1
    puntaje <- newIORef 1.0
    terminado <- newIORef 0
    displayCallback $= desplegar posicionx posiciony dispara enemigos tiempo randind puntajeM terminado
    idleCallback $= Just (temporizador tiempo posicionx posiciony vposx vposy dispara enemigos frecenem randind dfrecenem terminado)
    keyboardMouseCallback $= Just (teclado vposx vposy dispara posicionx posiciony terminado puntajeM randind)
    mainLoop

desplegar posicionx posiciony dispara enemigos tiempo puntaje puntajeM terminado= do
  term <- get terminado
  if term == 0 then do
    px <- get posicionx
    py <- get posiciony
    en <- get enemigos
    b <- get dispara
    t <- get tiempo
    p <- get puntaje
    pm <- get puntajeM
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
    currentRasterPosition $= Vertex4 (-1.0) 0.9 (-0.20) 1.0
    renderString TimesRoman24 $ "Puntos: " ++ show(p)
    currentRasterPosition $= Vertex4 (-1.0) 0.7 (-0.20) 1.0
    renderString TimesRoman24 $ "Record: " ++ show(pm)
    matrixMode $= Projection
    perspective 60 1 1 5 
    --perspective fov aspect zNear zFar
    --lookAt (position camera) (lookAt camera) (upVector camera)
    lookAt (Vertex3 px py 0) (Vertex3 px py 5) (Vector3 0 1 0)
    flush
    preservingMatrix $ do
        translate $ Vector3 (-0.01) (-0.05) (0::GLfloat)
        dibujaEntorno
        dibujaCubos en
        dibujaBalas b
    swapBuffers
  else do
    loadIdentity
    punt <- get puntaje
    matrixMode $= Modelview 0
    clear [ColorBuffer, DepthBuffer]
    currentRasterPosition $= Vertex4 (-0.2) 0.5 (-0.20) 1.0
    renderString TimesRoman24 $ "Ha perdido"
    currentRasterPosition $= Vertex4 (-0.3) 0 (-0.20) 1.0
    renderString TimesRoman24 $ "Su puntaje: " ++ show (punt)
    currentRasterPosition $= Vertex4 (-0.5) (-0.5) (-0.20) 1.0
    renderString TimesRoman24 $ "Presione R para volver a jugar"
    matrixMode $= Projection
    perspective 60 1 1 5 
    lookAt (Vertex3 0 0 0) (Vertex3 0 0 5) (Vector3 0 1 0)
    flush
    swapBuffers

temporizador tiempo angulox anguloy vangulox vanguloy dispara enemigos frecenem randind dfrecenem terminado= do
  term <- get terminado
  if term == 0 then do
    ax <- get angulox
    ay <- get anguloy
    vx <- get vangulox
    vy <- get vanguloy
    t <- get tiempo
    bl <- get dispara
    en <- get enemigos
    fr <- get frecenem
    ind <- get randind
    dfr <- get dfrecenem
    if t>=0.5 then do
        tiempo $= 0.0
        angulox $= ax + vx
        anguloy $= ay + vy
        terminado $= muere en
        dispara $= actualizaBala bl
        enemigos $= actualizaCubo en
        if fr>=1.0 then do
            frecenem $= 0
            let ranX = realToFrac (aleatorind ind)
            let ranY = realToFrac (aleatorind ind)
            randind $= ind + 1
            enemigos $= insertCubo ranX ranY 5 0.005 en
            dfrecenem $= dfr + 0.0005
        else do
            frecenem $= fr + dfr
        postRedisplay Nothing
    else do
        dispara $= destruyeBalas bl
        enemigos $= colisionar bl en
        tiempo $= t+_TIEMPO
        return ()
  else do
    angulox $= 0
    anguloy $= 0
    vangulox $= 0
    vanguloy $= 0
    tiempo $= 0
    dispara $= NullB
    enemigos $= NullC
    frecenem $= 0
    dfrecenem $= 0.001
    return ()

teclado vposx vposy dispara posicionx posiciony terminado puntajeM randind(Char 'a') Down _ _ = do
    vposx $= 0.02
    postRedisplay Nothing
teclado vposx vposy dispara posicionx posiciony terminado puntajeM randind(Char 'd') Down _ _ = do
    vposx $= -0.02
    postRedisplay Nothing
teclado vposx vposy dispara posicionx posiciony terminado puntajeM randind(Char 'w') Down _ _ = do
    vposy $= 0.02
    postRedisplay Nothing
teclado vposx vposy dispara posicionx posiciony terminado puntajeM randind(Char 's') Down _ _ = do
    vposy $= -0.02
    postRedisplay Nothing
teclado vposx vposy dispara posicionx posiciony terminado puntajeM randind(Char 'a') Up _ _ = do
    vposx $= 0
    postRedisplay Nothing
teclado vposx vposy dispara posicionx posiciony terminado puntajeM randind(Char 'd') Up _ _ = do
    vposx $= 0
    postRedisplay Nothing
teclado vposx vposy dispara posicionx posiciony terminado puntajeM randind(Char 'w') Up _ _ = do
    vposy $= 0
    postRedisplay Nothing
teclado vposx vposy dispara posicionx posiciony terminado puntajeM randind(Char 's') Up _ _ = do
    vposy $= 0
    postRedisplay Nothing
teclado vposx vposy dispara posicionx posiciony terminado puntajeM randind(Char 'k') Down _ _ = do
    vx <- get vposx
    vy <- get vposy
    vposy $= vy * 2
    vposx $= vx * 2
    postRedisplay Nothing
teclado vposx vposy dispara posicionx posiciony terminado puntajeM randind(Char 'r') Down _ _ = do
    trm <- get terminado
    if trm == 1 then do
        terminado $= 0
        rnd <- get randind
        pm <- get puntajeM
        if rnd > pm then do
            pnt <- get randind
            puntajeM $= pnt
            randind $= 0
        else do
            randind $= 0
        postRedisplay Nothing
    else do
        postRedisplay Nothing
teclado vposx vposy dispara posicionx posiciony terminado puntajeM randind(Char 'j') Down _ _ = do
    e <- get dispara
    x <- get posicionx
    y <- get posiciony
    let px = realToFrac (x)
    let py = realToFrac (y)
    dispara $= insertBala px py 1.0 0.05 e
    postRedisplay Nothing
teclado _ _ _ _ _ _ _ _ _ _ _ _= return ()

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

muere :: Cubos -> Integer
muere NullC = 0
muere (Cubo x y z v sig) = if z > 1 then muere sig else 1

dibujaCubos NullC = postRedisplay Nothing
dibujaCubos (Cubo posx posy posz v sig) = do
    let x = realToFrac posx
    let y = realToFrac posy
    let z = realToFrac posz
    materialDiffuse Front $= Color4 0.1 0.1 1 1
    superficie [(x-0.1,y-0.1,z), (x-0.1, y+0.1, z), (x+0.1, y+0.1, z), (x+0.1, y-0.1, z)] (0,0,-1)
    dibujaCubos sig

dibujaEntorno = do
    materialDiffuse Front $= Color4 1 0.95 0.95 1
    superficie [(1.3,1.3,0), (1.3,1.3,5), (1.29,1.31,5.01),(1.29,1.31,0)] (0,1,0)
    superficie [(1.3,-1.3,0), (1.3,-1.3,5), (1.29,-1.31,5.01),(1.29,-1.31,0)] (0,1,0)
    superficie [(-1.3,-1.3,0), (-1.3,-1.3,5), (-1.29,-1.29,5.01),(-1.29,-1.31,0)] (0,1,0)
    superficie [(-1.3,1.3,0), (-1.3,1.3,5), (-1.29,1.31,5.01),(-1.29,1.31,0)] (0,1,0)
    superficie [(-1.3,1.3,4.99), (1.3,1.3,4.99), (1.32,1.32,4.99),(-1.32,1.32,4.99)] (0,1,0)
    superficie [(1.3,1.3,4.99), (1.3,-1.3,4.99), (1.32,-1.32,4.99),(1.32,1.32,4.99)] (0,1,0)
    superficie [(1.3,-1.3,4.99), (-1.3,-1.3,4.99), (-1.32,-1.32,4.99),(1.32,-1.32,4.99)] (0,1,0)
    superficie [(-1.3,-1.3,4.99), (-1.3,1.3,4.99), (-1.32,1.32,4.99),(-1.32,-1.32,4.99)] (0,1,0)

aleatorios :: Float
aleatorios = do
  let aleatorios = randomRs (-1.0,1.0) (unsafePerformIO newStdGen) :: [Float]
  numeroAleatorio aleatorios

numeroAleatorio :: [Float]->Float
numeroAleatorio (x:xs) = x

aleatorind :: Int -> Float
aleatorind ind = do
  let aleatorios = randomRs (-1.0,1.0) (unsafePerformIO newStdGen) :: [Float]
  aleatorios!!ind

colisionar:: Balas -> Cubos -> Cubos
colisionar NullB cubs = cubs
colisionar _ NullC = NullC
colisionar (Bala bx by bz bv bsig) cubi = colisionar bsig (colisionesb (Bala bx by bz bv bsig) cubi)

colisionesb :: Balas -> Cubos -> Cubos
colisionesb NullB cubs = cubs
colisionesb _ NullC = NullC
colisionesb (Bala bx by bz bv bsig) (Cubo cx cy cz cv csig) = if (bx <= cx + 0.11) && (bx >= cx -0.11) && (by <= cy + 0.11) && (by >= cy - 0.11) && (bz >= cz) then colisionesb (Bala bx by bz bv bsig) csig else Cubo cx cy cz cv (colisionesb (Bala bx by bz bv bsig) csig)