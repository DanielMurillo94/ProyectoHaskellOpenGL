a{-# LANGUAGE FlexibleContexts #-}
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
    posicionx <- newIORef 0.0
    posiciony <- newIORef 0.0
    vposx <- newIORef 0.0
    vposy <- newIORef 0.0
    dispara <- newIORef 0
    displayCallback $= desplegar posicionx posiciony
    idleCallback $= Just (temporizador tiempo posicionx posiciony vposx vposy)
    keyboardMouseCallback $= Just (teclado vposx vposy)
    mainLoop

desplegar posicionx posiciony = do
    px <- get posicionx
    py <- get posiciony
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
        translate $ Vector3 (-0.25) (-0.25) (0::GLfloat)
        cubo 0.1 0 0 2
        puntero 0.1 px py 5 1
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

teclado vposx vposy (Char 'a') Down _ _ = do
    vposx $= 0.005
    postRedisplay Nothing
teclado vposx vposy (Char 'd') Down _ _ = do
    vposx $= -0.005
    postRedisplay Nothing
teclado vposx vposy (Char 'w') Down _ _ = do
    vposy $= 0.005
    postRedisplay Nothing
teclado vposx vposy (Char 's') Down _ _ = do
    vposy $= -0.005
    postRedisplay Nothing
teclado vposx vposy (Char 'a') Up _ _ = do
    vposx $= 0
    postRedisplay Nothing
teclado vposx vposy (Char 'd') Up _ _ = do
    vposx $= 0
    postRedisplay Nothing
teclado vposx vposy (Char 'w') Up _ _ = do
    vposy $= 0
    postRedisplay Nothing
teclado vposx vposy (Char 's') Up _ _ = do
    vposy $= 0
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
    superficie [(px-tam,py-tam,pz), (px-tam, py+tam, pz), (px+tam, py+tam, pz), (px+tam, py -tam, pz)] (0,0,-1)

superficie :: [(GLfloat, GLfloat, GLfloat)] -> (GLfloat,GLfloat,GLfloat) -> IO ()
superficie vertices (nx,ny,nz) = do
    renderPrimitive Polygon $ do
        normal (Normal3 nx ny nz)
        mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z) vertices