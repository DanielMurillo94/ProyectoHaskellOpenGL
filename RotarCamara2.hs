{-# LANGUAGE FlexibleContexts #-}
import Graphics.UI.GLUT
import Data.IORef
import GHC.Generics

_TIEMPO=0.1

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
    tiempo <- newIORef 0.0
    angulo <- newIORef 0.0
    angulox <- newIORef 0.0
    anguloy <- newIORef 0.0
    vangulox <- newIORef 0.0
    vanguloy <- newIORef 0.0
    displayCallback $= desplegar angulox anguloy
    idleCallback $= Just (temporizador tiempo angulox anguloy vangulox vanguloy)
    keyboardMouseCallback $= Just (teclado vangulox vanguloy)
    mainLoop

desplegar angulox anguloy = do
    ax <- get angulox
    ay <- get anguloy
    let vx = (cos ax)
    let vy = (sin ax)
    let vz = (sin ay)
    if ay > 1.3 then do 
         anguloy $= 1.3 
         ay <- get anguloy
         anguloy $= 1.3 
    else if ay < -1.3 then do 
         anguloy $= -1.3 
         ay <- get anguloy
         anguloy $= -1.3 
    else
        anguloy $= ay
    if ax > 1.3 then do 
         angulox $= 1.3 
         ax <- get angulox
         angulox $= 1.3 
    else if ax < -1.57 then do 
         angulox $= -1.3 
         ax <- get angulox
         angulox $= -1.3 
    else
        angulox $= ax
    
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
    lookAt (Vertex3 vx vy vz) (Vertex3 0 0 0) (Vector3 0 1 0)

    flush
    preservingMatrix $ do
        translate $ Vector3 (-0.25) (-0.25) (0::GLfloat)
        cubo 0.1 1 1 1
        puntero 0.1 vx vy vz 0.1
        puntero 0.1 vx vy vz 0.2
        puntero 0.1 vx vy vz 0.3
        puntero 0.1 vx vy vz 0.4
        puntero 0.1 vx vy vz 0.5
        puntero 0.1 vx vy vz 0.6
        puntero 0.1 vx vy vz 0.7
        puntero 0.1 vx vy vz 0.8
        puntero 0.1 vx vy vz 0.9
    swapBuffers

temporizador tiempo angulox anguloy vangulox vanguloy = do
    ax <- get angulox
    ay <- get anguloy
    vx <- get vangulox
    vy <- get vanguloy
    t <- get tiempo
    if t>=1.0 then do
        tiempo $= 0.0
        angulox $= ax + vx
        anguloy $= ay + vy
        postRedisplay Nothing
    else do
        tiempo $= t+_TIEMPO
        return ()

teclado vangulox vanguloy (Char 'a') Down _ _ = do
    vanguloy $= -0.005
    postRedisplay Nothing
teclado vangulox vanguloy (Char 'd') Down _ _ = do
    vanguloy $= 0.005
    postRedisplay Nothing
teclado vangulox vanguloy (Char 'w') Down _ _ = do
    vangulox $= 0.005
    postRedisplay Nothing
teclado vangulox vanguloy (Char 's') Down _ _ = do
    vangulox $= -0.005
    postRedisplay Nothing
teclado vangulox vanguloy (Char 'a') Up _ _ = do
    vanguloy $= 0
    postRedisplay Nothing
teclado vangulox vanguloy (Char 'd') Up _ _ = do
    vanguloy $= 0
    postRedisplay Nothing
teclado vangulox vanguloy (Char 'w') Up _ _ = do
    vangulox $= 0
    postRedisplay Nothing
teclado vangulox vanguloy (Char 's') Up _ _ = do
    vangulox $= 0
    postRedisplay Nothing
teclado _ _ _ _ _ _= return ()

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
    materialDiffuse Front $= Color4 1 0 0 1
    superficie [(0+px,0+py,0+pz), (tam+px, 0+py, 0+pz), (tam+px, tam+py, 0+pz), (0+px, tam+py, 0+pz)] (0,0,-1)
    superficie [(0+px,0+py,0+pz), (0+px, tam+py, 0+pz), (0+px, tam+py, tam+pz), (0+px, 0+py, tam+pz)] (-1,0,0)
    superficie [(0+px, 0+py, tam+pz), (tam+px, 0+py, tam+pz), (tam+px, tam+py, tam+pz), (0+px, tam+py, tam+pz)] (0,0,1)
    superficie [(tam+px, 0+py, 0+pz), (tam+px, tam+py, 0+pz), (tam+px, tam+py, tam+pz), (tam+px, 0+py, tam+pz)] (1,0,0)
    superficie [(0+px, 0+py, 0+pz), (tam+px, 0+py, 0+pz), (tam+px, 0+py, tam+pz), (0+px, 0+py, tam+pz)] (0,-1,0)
    superficie [(0+px, tam+py, 0+pz), (tam+px, tam+py, 0+pz), (tam+px, tam+py, tam+pz), (0+px, tam+py, tam+pz)] (0,1,0)

superficie :: [(GLfloat, GLfloat, GLfloat)] -> (GLfloat,GLfloat,GLfloat) -> IO ()
superficie vertices (nx,ny,nz) = do
    renderPrimitive Polygon $ do
        normal (Normal3 nx ny nz)
        mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z) vertices