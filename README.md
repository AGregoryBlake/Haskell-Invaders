# Haskell-Invaders
Tested on Ubuntu 15.10 with SDL 2.0.4

To install without static links:
1) Clone the repository.
2) Install SDL 2.0.4 (https://www.libsdl.org/download-2.0.php)
3) Install Haskell Stack 1.0.4 (http://docs.haskellstack.org/en/stable/GUIDE.html)
4) From the local Haskell-Invaders workspace, type "stack build", "stack exec space-invaders-exe"

I based my implementation off of a friend's CS2500 assignment and as a result, the invaders do not move. (http://www.ccs.neu.edu/course/cs2500f13-seattle/assignments/assignment7.html)

Also, you've got to hit an invader's corner in order to hit them, and the ship is not hit unless it's an upper left corner -- This was a learning project and I'd rather be working on the next project than hammering out a box-detection algorithm for a project that I consider unlikely for others to pull and install. If you're disappointed with this implementation, by all means play the actual game here: www.freeinvaders.org
