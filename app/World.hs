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

worldUpdate :: World -> Direction -> Bool -> World
worldUpdate world dir firing
            | firing = worldMove $ shipFire $ changeDirection world dir
            | otherwise = worldMove $ changeDirection world dir
          
worldMove :: World -> World
worldMove (World ship invaders shipBullets invaderBullets) = World (shipMove ship) invaders (bulletsMove shipBullets) (bulletsMove shipBullets)

bulletsMove :: [Bullet] -> [Bullet]
bulletsMove bullets = map (\(Bullet dir (P (V2 x y))) -> if dir == DUp then Bullet dir (P $ V2 x (y - bulletSpeed)) else Bullet dir (P $ V2 x (y + bulletSpeed))) bullets

shipMove :: Ship -> Ship
shipMove (Ship dir (P (V2 x y)))
         | dir == DLeft = Ship dir (P $ V2 (x - shipSpeed) y)
         | otherwise = Ship dir (P $ V2 (x + shipSpeed) y)

-- invadersFire :: [Invader] -> [Bullet] -> [Bullet]
-- removeHitsAndOutOfBounds
-- shipHit

shipFire :: World -> World
shipFire world@(World ship invaders shipBullets invaderBullets)
         | length shipBullets >= 3 = world
         | otherwise = World ship invaders ((Bullet DUp (getShipLocation world)) : shipBullets) invaderBullets
         
changeDirection :: World -> Direction -> World
changeDirection (World (Ship _ (P (V2 x y))) invaders shipBullets invaderBullets) dir = World (Ship dir (P $ V2 x y)) invaders shipBullets invaderBullets
             
getShipDirection :: World -> Direction
getShipDirection (World (Ship dir _) _ _ _) = dir                 

getShipLocation :: World -> Location
getShipLocation (World (Ship _ p) _ _ _) = p                
