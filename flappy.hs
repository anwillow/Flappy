import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

windowWidth, windowHeight :: Int
windowWidth = 900
windowHeight = 600

pipeWidth :: Float
pipeWidth = 50

gravity :: Float
gravity = -500

groundHeight :: Float
groundHeight = 70

cloudHeight :: Float
cloudHeight = 70

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
initialState gen = do
    let pipes = generateInitialPipes gen
    return GameState {
        birdY = 0,
        birdVelocity = 0,
        pipes = pipes,
        gameOver = False
    }

generateInitialPipes :: StdGen -> [(Float, Float)]
generateInitialPipes gen =
    let (gen1, gen2) = split gen
        pipeGap = 200
        minHeight = 100
        maxHeight = fromIntegral windowHeight - groundHeight - cloudHeight - pipeGap - minHeight
        (topHeight, gen') = randomR (minHeight, maxHeight) gen1
        (gapSize, gen'') = randomR (80 :: Int, 110 :: Int) gen2
        bottomHeight = fromIntegral windowHeight - groundHeight - topHeight - pipeGap - fromIntegral gapSize
    in [(fromIntegral windowWidth, topHeight), (fromIntegral windowWidth, -bottomHeight)]

main :: IO ()
main = do
    gen <- getStdGen
    initialState <- initialState gen
    playIO (InWindow "Flappy Bird" (windowWidth, windowHeight) (100, 100))
           cyan
           60
           initialState
           (drawState gen)
           handleEvent
           (\dt gameState -> stepGame dt gameState gen)

drawState :: StdGen -> GameState -> IO Picture
drawState gen gameState
    | gameOver gameState = drawGameOverScreen
    | otherwise = drawGameScreen gameState

drawGameScreen :: GameState -> IO Picture
drawGameScreen (GameState birdY _ pipes _) = do
    let
        pipePictures = map (\(x, h) -> drawPipe x h) pipes
        bird = translate 0 birdY $ color yellow $ circleSolid 20
        ground = translate 0 (-fromIntegral windowHeight / 2 + groundHeight / 2) $ color brown $ rectangleSolid (fromIntegral windowWidth) groundHeight
        cloud = translate 0 (fromIntegral windowHeight / 2 - cloudHeight / 2) $ color white $ rectangleSolid (fromIntegral windowWidth) cloudHeight
    return $ pictures (bird : pipePictures ++ [ground, cloud])

drawGameOverScreen :: IO Picture
drawGameOverScreen = do
    let
        gameOverText = translate (-200) 0 $ scale 0.2 0.2 $ color red $ text "Game Over"
        restartText = translate (-300) (-100) $ scale 0.4 0.4 $ color blue $ text "Press Space to Restart"
    return $ pictures [gameOverText, restartText]

drawPipe :: Float -> Float -> Picture
drawPipe x height =
    let
        halfWidth = pipeWidth / 2
        upperPipe = translate x (height / 2) $ color myGreen $ rectangleSolid pipeWidth (fromIntegral windowHeight - height)
    in
        pictures [upperPipe]

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

stepGame :: Float -> GameState -> StdGen -> IO GameState
stepGame dt gameState@(GameState birdY birdVelocity pipes gameOver) gen
    | gameOver = return gameState
    | otherwise = do
        let
            newBirdY = birdY + birdVelocity * dt
            newVelocity = birdVelocity + gravity * dt
            (newPipes, gen') = updatePipes dt pipes gen
            collision = checkCollision (birdY, newBirdY)
            gameOver' = if collision then True else gameOver
        return $ gameState { birdY = newBirdY, birdVelocity = newVelocity, pipes = newPipes, gameOver = gameOver' }

checkCollision :: (Float, Float) -> Bool
checkCollision (oldY, newY) = newY <= (- fromIntegral windowHeight / 2 + groundHeight / 2) || newY >= (fromIntegral windowHeight / 2 - cloudHeight / 2)

updatePipes :: Float -> [(Float, Float)] -> StdGen -> ([(Float, Float)], StdGen)
updatePipes dt pipes gen = (map (\(x, h) -> (x - pipeSpeed * dt, h)) pipes, gen')
    where
        pipeSpeed = 100
        (gen', _) = split gen
