import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

windowWidth, windowHeight :: Int
windowWidth = 900
windowHeight = 600

gravity :: Float
gravity = -500

groundHeight :: Float
groundHeight = 70

cloudHeight :: Float
cloudHeight = 70

pipeWidth :: Float
pipeWidth = 70

brown :: Color
brown = makeColor (139/255) (69/255) (19/255) 1

myGreen :: Color
myGreen = makeColor (34/255) (139/255) (34/255) 1

data GameState = GameState {
    birdY :: Float,
    birdVelocity :: Float,
    pipes :: [(Float, Float)],
    gameOver :: Bool
}

initialState :: StdGen -> IO GameState
initialState _ = return GameState {
    birdY = 0,
    birdVelocity = 0,
    pipes = initialPipes,
    gameOver = False
}
  where
    initialPipes = [(fromIntegral windowWidth / 2, fromIntegral windowHeight / 4)]

main :: IO ()
main = do
    gen <- getStdGen
    initialState <- initialState gen
    playIO (InWindow "Flappy Bird" (windowWidth, windowHeight) (100, 100))
           cyan
           60
           initialState
           drawState
           handleEvent
           stepGame

drawState :: GameState -> IO Picture
drawState gameState
    | gameOver gameState = drawGameOverScreen
    | otherwise = drawGameScreen gameState

drawGameScreen :: GameState -> IO Picture
drawGameScreen (GameState birdY _ pipes gameOver) = do
    let
        bird = translate 0 birdY $ color yellow $ circleSolid 20
        ground = translate 0 (-fromIntegral windowHeight / 2 + groundHeight / 2) $ color brown $ rectangleSolid (fromIntegral windowWidth) groundHeight
        cloud = translate 0 (fromIntegral windowHeight / 2 - cloudHeight / 2) $ color white $ rectangleSolid (fromIntegral windowWidth) cloudHeight
        drawPipe (x, gapY) = pictures [ translate x (gapY / 2) $ color green $ rectangleSolid pipeWidth (fromIntegral windowHeight - gapY - groundHeight),
                                        translate x (- (gapY / 2)) $ color green $ rectangleSolid pipeWidth (fromIntegral windowHeight - gapY - groundHeight),
                                        translate x 0 $ color cyan $ rectangleSolid pipeWidth 80]
        pipesPictures = mconcat $ map drawPipe pipes
    return $ pictures [pipesPictures, bird, cloud, ground]

drawGameOverScreen :: IO Picture
drawGameOverScreen = do
    let
        gameOverText = translate (-200) 0 $ scale 0.2 0.2 $ color red $ text "Game Over"
        restartText = translate (-300) (-100) $ scale 0.4 0.4 $ color blue $ text "Press Space to Restart"
    return $ pictures [gameOverText, restartText]

handleEvent :: Event -> GameState -> IO GameState
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) gameState
    | gameOver gameState = restartGame
    | otherwise = flapBird gameState
handleEvent _ gameState = return gameState

flapBird :: GameState -> IO GameState
flapBird gameState
    | gameOver gameState = return gameState
    | otherwise = return $ gameState { birdVelocity = 300 }

restartGame :: IO GameState
restartGame = do
    gen <- getStdGen
    initialState gen

stepGame :: Float -> GameState -> IO GameState
stepGame dt gameState@(GameState birdY birdVelocity pipes gameOver)
    | gameOver = return gameState
    | otherwise = do
        let
            newBirdY = birdY + birdVelocity * dt
            newVelocity = birdVelocity + gravity * dt
            newPipes = movePipes pipes dt
            collision = checkCollision newBirdY pipes
            gameOver' = if collision then True else gameOver
        return $ gameState { birdY = newBirdY, birdVelocity = newVelocity, pipes = newPipes, gameOver = gameOver' }

checkCollision :: Float -> [(Float, Float)] -> Bool
checkCollision birdY pipes =
    let
        birdTopY = birdY + 20
        birdBottomY = birdY - 20
        groundTopY = -fromIntegral windowHeight / 2 + groundHeight / 2
        cloudTopY = fromIntegral windowHeight / 2 - cloudHeight / 2
        cloudBottomY = cloudTopY - cloudHeight
        topPipeTops = map (\(_, gapY) -> gapY / 2) pipes
        bottomPipeBottoms = map (\(_, gapY) -> -gapY / 2) pipes
        pipeLeftXs = map (\(x, _) -> x - pipeWidth / 2) pipes
        pipeRightXs = map (\(x, _) -> x + pipeWidth / 2) pipes
        birdLeftX = -20
        birdRightX = 20
        maxBirdTopY = maximum (birdTopY : topPipeTops)
        minBirdBottomY = minimum (birdBottomY : bottomPipeBottoms)
        birdCollidesWithPipes = birdLeftX <= maximum pipeRightXs && birdRightX >= minimum pipeLeftXs &&
                                (birdTopY >= maxBirdTopY || birdBottomY <= minBirdBottomY)
        birdCollidesWithGround = birdY <= groundTopY
        birdCollidesWithClouds = birdY >= cloudBottomY
    in
        birdCollidesWithPipes || birdCollidesWithGround || birdCollidesWithClouds

movePipes :: [(Float, Float)] -> Float -> [(Float, Float)]
movePipes pipes dt = map (\(x, gapY) -> (x - dt * 100, gapY)) pipes
