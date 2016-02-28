module World where
        
import Linear
import Linear.Affine
import Foreign.C.Types
       
data Direction = DLeft | DRight | DUp | DDown deriving (Eq)
type Location = Point V2 CInt
data Invader = Invader Location
data Bullet = Bullet Direction Location
data Ship = Ship Direction Location

data World = World Ship [Invader] [Bullet] [Bullet]

maxShipBullets = 3
maxInvaderBullets = 15
shipSpeed = 5
bulletSpeed = 5

shipInit :: Ship
shipInit = Ship DLeft (P $ V2 (CInt 250) (CInt 480))

invadersInit :: [Invader]
invadersInit = [Invader (P $ V2 (CInt x) (CInt y)) |
             x <- [100, 140, 180, 220, 260, 300, 340, 380, 420],
             y <- [20, 50, 80, 110]]

worldInit :: World
worldInit = World shipInit invadersInit [] []            

worldUpdate :: Int -> World -> Direction -> Bool -> World
worldUpdate a world dir firing
            | firing = worldMove $ shipFire $ invadersFire a $ removeHitsAndOutOfBounds $ changeDirection world dir
            | otherwise = worldMove $ invadersFire a $ removeHitsAndOutOfBounds $ changeDirection world dir
          
worldMove :: World -> World
worldMove (World ship invaders shipBullets invaderBullets) = World (shipMove ship) invaders (bulletsMove shipBullets) (bulletsMove invaderBullets)

bulletsMove :: [Bullet] -> [Bullet]
bulletsMove bullets = map (\(Bullet dir (P (V2 x y))) -> if dir == DUp then Bullet dir (P $ V2 x (y - bulletSpeed)) else Bullet dir (P $ V2 x (y + bulletSpeed))) bullets

shipMove :: Ship -> Ship
shipMove (Ship dir (P (V2 x y)))
         | dir == DLeft = Ship dir (P $ V2 (squash (x - shipSpeed)) y)
         | otherwise = Ship dir (P $ V2 (squash (x + shipSpeed)) y)
         where squash n = (n + 500) `mod` 500

invadersFire :: Int -> World -> World
invadersFire a (World ship invaders shipBullets invaderBullets) = World ship invaders shipBullets (invaderFires (invaders !! a) invaderBullets)

invaderFires :: Invader -> [Bullet] -> [Bullet]
invaderFires invader invaderBullets
             | length invaderBullets >= maxInvaderBullets = invaderBullets
             | otherwise = (Bullet DDown (getInvaderLocation invader)) : invaderBullets
             
removeHitsAndOutOfBounds :: World -> World
removeHitsAndOutOfBounds (World ship invaders shipBullets invaderBullets) = 
        World ship (removeCollidedInvaders invaders shipBullets) (removeCollidedBullets (removeOutOfBounds shipBullets) invaders) (removeOutOfBounds invaderBullets)

removeCollidedInvaders :: [Invader] -> [Bullet] -> [Invader]
removeCollidedInvaders invaders bullets = filter (\(Invader l) -> l `notElem` (map getBulletLocation bullets)) invaders

removeCollidedBullets :: [Bullet] -> [Invader] -> [Bullet]
removeCollidedBullets bullets invaders = filter (\bullet -> (getBulletLocation bullet) `notElem` (map getInvaderLocation invaders)) bullets

removeOutOfBounds :: [Bullet] -> [Bullet]
removeOutOfBounds bullets = filter (\bullet -> (getY $ getBulletLocation bullet) > 0 && (getY $ getBulletLocation bullet) < 500) bullets
                  where getY (P (V2 _ y)) = y

shipHit :: World -> Bool
shipHit world = (getShipLocation world) `elem` (getInvaderBulletLocations world)

shipFire :: World -> World
shipFire world@(World ship invaders shipBullets invaderBullets)
                      | length shipBullets >= maxShipBullets = world
                      | otherwise = World ship invaders ((Bullet DUp (getShipLocation world)) : shipBullets) invaderBullets
         
changeDirection :: World -> Direction -> World
changeDirection (World (Ship _ (P (V2 x y))) invaders shipBullets invaderBullets) dir = World (Ship dir (P $ V2 x y)) invaders shipBullets invaderBullets
             
getShipDirection :: World -> Direction
getShipDirection (World (Ship dir _) _ _ _) = dir                 

getShipLocation :: World -> Location
getShipLocation (World (Ship _ p) _ _ _) = p                

getInvaderBulletLocations :: World -> [Location]
getInvaderBulletLocations (World _ _ _ invaderBullets) = map getBulletLocation invaderBullets

getBulletLocation :: Bullet -> Location
getBulletLocation (Bullet _ l) = l

getInvaderLocation :: Invader -> Location
getInvaderLocation (Invader l) = l

gameOver :: World -> Bool
gameOver world = shipHit world || invadersGone world

invadersGone :: World -> Bool
invadersGone (World _ invaders _ _) = (length invaders) == 0

getInvaders :: World -> [Invader]
getInvaders (World _ invaders _ _) = invaders            
