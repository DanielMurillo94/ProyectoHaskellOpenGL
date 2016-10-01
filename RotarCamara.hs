{-# LANGUAGE FlexibleContexts #-}
import Graphics.UI.GLUT
import Data.IORef

_TIEMPO=0.01

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
    loadIdentity
    matrixMode $= Modelview 0
    clear [ColorBuffer, DepthBuffer]
    currentRasterPosition $= Vertex4 (-1.0) 0.9 (-0.20) 1.0
    renderString TimesRoman24 $ "AnguloX: " ++ show(ax)
    currentRasterPosition $= Vertex4 (-1.0) 0.5 (-0.20) 1.0
    renderString TimesRoman24 $ "AnguloY: " ++ show(ay)
    matrixMode $= Projection
    rotate ax $ Vector3 1 0 (0.0 ::GLfloat)
    rotate ay $ Vector3 0 1 (0.0 ::GLfloat)
    perspective 60 1 1 5
    lookAt (Vertex3 0 0 0) (Vertex3 0 0 1) (Vector3 0 1 0)

    flush
    preservingMatrix $ do
        translate $ Vector3 (-0.25) (-0.25) (0::GLfloat)
        cubo 0.1 0 0 0.9
        cubo 0.1 0.9 0 0.9
        cubo 0.1 0.9 0 0
        cubo 0.1 0 0 (-0.9)
        cubo 0.1 0.9 0 (-0.9)
        cubo 0.1 0.9 0 0
        cubo 0.1 0 0 0.9
        cubo 0.1 (-0.9) 0 0.9
        cubo 0.1 (-0.9) 0 0
        cubo 0.1 (-0.9) 0 (-0.9)
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
    vanguloy $= -1
    postRedisplay Nothing
teclado vangulox vanguloy (Char 'd') Down _ _ = do
    vanguloy $= 1
    postRedisplay Nothing
teclado vangulox vanguloy (Char 'w') Down _ _ = do
    vangulox $= 1
    postRedisplay Nothing
teclado vangulox vanguloy (Char 's') Down _ _ = do
    vangulox $= -1
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

superficie :: [(GLfloat, GLfloat, GLfloat)] -> (GLfloat,GLfloat,GLfloat) -> IO ()
superficie vertices (nx,ny,nz) = do
    renderPrimitive Polygon $ do
        normal (Normal3 nx ny nz)
        mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z) vertices