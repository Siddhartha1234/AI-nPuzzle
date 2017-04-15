module GraphicsUtils where

import Graphics.UI.GLUT
import Graphics.GLUtil
import Data.IORef
 
vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z
 
cube :: GLfloat -> IO ()
cube w = renderPrimitive Quads $ mapM_ vertex3f
  [ ( w, w, w), ( w, w,-w), ( w,-w,-w), ( w,-w, w),
    ( w, w, w), ( w, w,-w), (-w, w,-w), (-w, w, w),
    ( w, w, w), ( w,-w, w), (-w,-w, w), (-w, w, w),
    (-w, w, w), (-w, w,-w), (-w,-w,-w), (-w,-w, w),
    ( w,-w, w), ( w,-w,-w), (-w,-w,-w), (-w,-w, w),
    ( w, w,-w), ( w,-w,-w), (-w,-w,-w), (-w, w,-w) ]

points :: Int -> Int -> Int -> [(GLfloat , GLfloat , GLfloat)]
points r 1 n = [( -1.0 + 1.0/n1, 1.0 - 1.0/n1 - 2.0*((k-1))/n1 ,0) | k <- [1..n1]]
			   where 
			   		n1 = fromIntegral n	
points r c n = (points r (c-1) n) ++ [(-1.0 + 1.0/n1 + 2.0*(fromIntegral (c-1))/n1, 1.0 - 1.0/n1 - 2.0*((k-1))/n1 ,0) | k <- [1..n1]]
			   where 
			   		n1 = fromIntegral n

get_index_from_point :: (GLfloat , GLfloat) -> GLfloat  -> (Int , Int) 
get_index_from_point (x,y) _size = (floor ((1 +(1-y)*_size)/2) , floor ((1 + (x+1)*_size)/2))

draw_line :: (GLfloat , GLfloat) ->  (GLfloat , GLfloat) -> IO()
draw_line (x,y) (a,b) = do
	renderPrimitive Lines $ do
		vertex $ Vertex3 x y 0
		vertex $ Vertex3 a b 0 

loadGLTextureFromFile :: FilePath -> IO TextureObject
loadGLTextureFromFile f = do t <- either error id <$> readTexture f
                             textureFilter Texture2D $= ((Linear', Nothing), Linear')
                             texture2DWrap $= (Mirrored, ClampToEdge)
                             return t
              
drawTexture :: (GLfloat,GLfloat) -> (GLfloat,GLfloat) -> (GLfloat,GLfloat) -> (GLfloat,GLfloat) -> IO()
drawTexture (u,v) (uw,vh) (x,y) (w,h) = do
    renderPrimitive Quads $ do
        tex (u)      (v - vh) >> ver (x)     (y - h) -- Top left coor: (-1, 1)
        tex (u + uw) (v - vh) >> ver (x + w) (y - h)
        tex (u + uw) (v)      >> ver (x + w) (y)    
        tex (u)      (v)      >> ver (x)     (y)    
        where ver x y = vertex (Vertex2 x y :: Vertex2 GLfloat)
              tex u v = texCoord (TexCoord2 u v :: TexCoord2 GLfloat)

length_string :: String -> Int
length_string [] = 0
length_string (x:xs) = 1 + length_string(xs)

drawDigit :: String -> Int -> GLfloat -> GLfloat -> GLfloat -> IO()
drawDigit "" _ _ _ _ = return ()
drawDigit (x:xs) len xcurr ycurr _size = do
	if x == '0' then do
		draw_line (xcurr - 1.0/(_size*(fromIntegral(len) + 2)*2.5) ,ycurr + 1.0/(_size*4)) (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5) , ycurr + 1.0/(_size*4)) 
		draw_line (xcurr - 1.0/(_size*(fromIntegral(len) + 2)*2.5) ,ycurr - 1.0/(_size*4)) (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5) , ycurr - 1.0/(_size*4)) 
		draw_line (xcurr - 1.0/(_size*(fromIntegral(len) + 2)*2.5),ycurr) (xcurr - 1.0/(_size*(fromIntegral(len) + 2)*2.5), ycurr + 1.0/(_size*4))
		draw_line (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5),ycurr - 1.0/(_size*4)) (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5), ycurr)
		draw_line (xcurr - 1.0/(_size*(fromIntegral(len) + 2)*2.5),ycurr - 1.0/(_size*4)) (xcurr - 1.0/(_size*(fromIntegral(len) + 2)*2.5), ycurr)
		draw_line (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5),ycurr) (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5), ycurr + 1.0/(_size*4))
		drawDigit xs len (xcurr + 1.0/(_size*(fromIntegral(len) + 2)))  ycurr _size
	else if x == '1' then do
		draw_line (xcurr,ycurr - 1.0/(_size*4)) (xcurr , ycurr + 1.0/(_size*4))
		drawDigit xs len (xcurr + 1.0/(_size*(fromIntegral(len) + 2)))  ycurr _size 
	else if x == '2' then do
		draw_line (xcurr - 1.0/(_size*(fromIntegral(len) + 2)*2.5) ,ycurr + 1.0/(_size*4)) (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5) , ycurr + 1.0/(_size*4)) 
		draw_line (xcurr - 1.0/(_size*(fromIntegral(len) + 2)*2.5) ,ycurr - 1.0/(_size*4)) (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5) , ycurr - 1.0/(_size*4)) 
		draw_line (xcurr - 1.0/(_size*(fromIntegral(len) + 2)*2.5) ,ycurr) (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5) , ycurr) 
		draw_line (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5),ycurr) (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5), ycurr + 1.0/(_size*4))
		draw_line (xcurr - 1.0/(_size*(fromIntegral(len) + 2)*2.5),ycurr - 1.0/(_size*4)) (xcurr - 1.0/(_size*(fromIntegral(len) + 2)*2.5), ycurr)
		drawDigit xs len (xcurr + 1.0/(_size*(fromIntegral(len) + 2)))  ycurr _size
	else if x == '3' then do 
		draw_line (xcurr - 1.0/(_size*(fromIntegral(len) + 2)*2.5) ,ycurr + 1.0/(_size*4)) (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5) , ycurr + 1.0/(_size*4)) 
		draw_line (xcurr - 1.0/(_size*(fromIntegral(len) + 2)*2.5) ,ycurr - 1.0/(_size*4)) (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5) , ycurr - 1.0/(_size*4)) 
		draw_line (xcurr - 1.0/(_size*(fromIntegral(len) + 2)*2.5) ,ycurr) (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5) , ycurr) 
		draw_line (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5),ycurr) (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5), ycurr + 1.0/(_size*4))
		draw_line (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5),ycurr - 1.0/(_size*4)) (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5), ycurr)
		drawDigit xs len (xcurr + 1.0/(_size*(fromIntegral(len) + 2)))  ycurr _size
	else if x == '4' then do
		draw_line (xcurr - 1.0/(_size*(fromIntegral(len) + 2)*2.5),ycurr) (xcurr - 1.0/(_size*(fromIntegral(len) + 2)*2.5), ycurr + 1.0/(_size*4))
		draw_line (xcurr - 1.0/(_size*(fromIntegral(len) + 2)*2.5) ,ycurr) (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5) , ycurr) 
		draw_line (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5),ycurr) (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5), ycurr + 1.0/(_size*4))
		draw_line (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5),ycurr - 1.0/(_size*4)) (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5), ycurr)
		drawDigit xs len (xcurr + 1.0/(_size*(fromIntegral(len) + 2)))  ycurr _size
	else if x == '5' then do
		draw_line (xcurr - 1.0/(_size*(fromIntegral(len) + 2)*2.5) ,ycurr + 1.0/(_size*4)) (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5) , ycurr + 1.0/(_size*4)) 
		draw_line (xcurr - 1.0/(_size*(fromIntegral(len) + 2)*2.5) ,ycurr - 1.0/(_size*4)) (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5) , ycurr - 1.0/(_size*4)) 
		draw_line (xcurr - 1.0/(_size*(fromIntegral(len) + 2)*2.5) ,ycurr) (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5) , ycurr) 
		draw_line (xcurr - 1.0/(_size*(fromIntegral(len) + 2)*2.5),ycurr) (xcurr - 1.0/(_size*(fromIntegral(len) + 2)*2.5), ycurr + 1.0/(_size*4))
		draw_line (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5),ycurr - 1.0/(_size*4)) (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5), ycurr)
		drawDigit xs len (xcurr + 1.0/(_size*(fromIntegral(len) + 2)))  ycurr _size
	else if x == '6' then do
		draw_line (xcurr - 1.0/(_size*(fromIntegral(len) + 2)*2.5) ,ycurr + 1.0/(_size*4)) (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5) , ycurr + 1.0/(_size*4)) 
		draw_line (xcurr - 1.0/(_size*(fromIntegral(len) + 2)*2.5) ,ycurr - 1.0/(_size*4)) (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5) , ycurr - 1.0/(_size*4)) 
		draw_line (xcurr - 1.0/(_size*(fromIntegral(len) + 2)*2.5) ,ycurr) (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5) , ycurr) 
		draw_line (xcurr - 1.0/(_size*(fromIntegral(len) + 2)*2.5),ycurr) (xcurr - 1.0/(_size*(fromIntegral(len) + 2)*2.5), ycurr + 1.0/(_size*4))
		draw_line (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5),ycurr - 1.0/(_size*4)) (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5), ycurr)
		draw_line (xcurr - 1.0/(_size*(fromIntegral(len) + 2)*2.5),ycurr - 1.0/(_size*4)) (xcurr - 1.0/(_size*(fromIntegral(len) + 2)*2.5), ycurr)
		drawDigit xs len (xcurr + 1.0/(_size*(fromIntegral(len) + 2)))  ycurr _size
	else if x == '7' then do
		draw_line (xcurr - 1.0/(_size*(fromIntegral(len) + 2)*2.5) ,ycurr + 1.0/(_size*4)) (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5) , ycurr + 1.0/(_size*4)) 
		draw_line (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5),ycurr) (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5), ycurr + 1.0/(_size*4))
		draw_line (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5),ycurr - 1.0/(_size*4)) (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5), ycurr)
		drawDigit xs len (xcurr + 1.0/(_size*(fromIntegral(len) + 2)))  ycurr _size
	else if x == '8' then do
		draw_line (xcurr - 1.0/(_size*(fromIntegral(len) + 2)*2.5) ,ycurr + 1.0/(_size*4)) (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5) , ycurr + 1.0/(_size*4)) 
		draw_line (xcurr - 1.0/(_size*(fromIntegral(len) + 2)*2.5) ,ycurr - 1.0/(_size*4)) (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5) , ycurr - 1.0/(_size*4)) 
		draw_line (xcurr - 1.0/(_size*(fromIntegral(len) + 2)*2.5) ,ycurr) (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5) , ycurr) 
		draw_line (xcurr - 1.0/(_size*(fromIntegral(len) + 2)*2.5),ycurr) (xcurr - 1.0/(_size*(fromIntegral(len) + 2)*2.5), ycurr + 1.0/(_size*4))
		draw_line (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5),ycurr - 1.0/(_size*4)) (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5), ycurr)
		draw_line (xcurr - 1.0/(_size*(fromIntegral(len) + 2)*2.5),ycurr - 1.0/(_size*4)) (xcurr - 1.0/(_size*(fromIntegral(len) + 2)*2.5), ycurr)
		draw_line (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5),ycurr) (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5), ycurr + 1.0/(_size*4))
		drawDigit xs len (xcurr + 1.0/(_size*(fromIntegral(len) + 2)))  ycurr _size
	else do
		draw_line (xcurr - 1.0/(_size*(fromIntegral(len) + 2)*2.5) ,ycurr + 1.0/(_size*4)) (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5) , ycurr + 1.0/(_size*4)) 
		draw_line (xcurr - 1.0/(_size*(fromIntegral(len) + 2)*2.5) ,ycurr) (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5) , ycurr) 
		draw_line (xcurr - 1.0/(_size*(fromIntegral(len) + 2)*2.5),ycurr) (xcurr - 1.0/(_size*(fromIntegral(len) + 2)*2.5), ycurr + 1.0/(_size*4))
		draw_line (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5),ycurr - 1.0/(_size*4)) (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5), ycurr)
		draw_line (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5),ycurr) (xcurr + 1.0/(_size*(fromIntegral(len) + 2)*2.5), ycurr + 1.0/(_size*4))
		drawDigit xs len (xcurr + 1.0/(_size*(fromIntegral(len) + 2)))  ycurr _size