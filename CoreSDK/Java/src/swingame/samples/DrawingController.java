package swingame.samples;

import java.awt.Color;
import java.awt.Point;
import swingame.Core;
import swingame.Graphics;
import swingame.Input;
import swingame.Keys;
import swingame.MouseButton;

/**
 * The DrawingController is used be the user interface to control the actions performed on
 * a Drawing object. This is responsible for handling the user input and turning this into
 * actions is performs on the Drawing object.
 *
 * @author  Andrew Cain
 */
 public class DrawingController
 {
     private enum DrawingElements
     {
         LINE, RECTANGLE, ELLIPSE;
     }

     private final int _defaultWidth = 10;
     private final int _defaultHeight = 10;
     private static final Color _defaultColor = Color.RED;

     private DrawingElements _adding = DrawingElements.RECTANGLE;
     private Drawing _controlling;

     public Drawing getControlling()
     {
         return _controlling;
     }
     
     public void setControlling(Drawing value)
     {
         _controlling = value;
     }

     public DrawingController()
     {
         this(new Drawing());
     }

     public DrawingController(Drawing drawing)
     {
         _controlling = drawing;
     }

     public void selectShape(Point point)
     {
         _controlling.selectShape(point);
     }

     public void changeColor()
     {
         Shape s = _controlling.getSelectedShape();
         if (s != null)
         {
             float h = Core.getHue(s.getColor());
             
             System.out.println("Hue: " + h);
             
             h += 0.01f;
             if (h > 1) h = h - 1;

             s.setColor(Color.getHSBColor(h, 0.9f, 1.0f));
         }
     }

     public void changeSize(int dw, int dh)
     {
         Shape s = _controlling.getSelectedShape();
         if (s != null)
         {
             s.setWidth(s.getWidth() + dw);
             s.setHeight(s.getHeight() + dh);
         }
     }

     public void moveTo(Point point)
     {
         Shape s = _controlling.getSelectedShape();
         if (s != null)
         {
             s.setPosition(point);
         }            
     }

     public void move(int dx, int dy)
     {
         Shape s = _controlling.getSelectedShape();
         if (s != null)
         {
             Point p = s.getPosition();
             p.setLocation(p.getX() + dx, p.getY() + dy);
         }
     }

     public void addNewShape(Point point)
     {
         Shape s;
         switch (_adding)
         {
             case RECTANGLE: s = new Rectangle(); break;
             case LINE: s = new Line(); break;
             default: return;
         }

         Point p = new Point();
         p.setLocation(point.getX() - _defaultWidth / 2, point.getY() - _defaultHeight / 2);

         s.setPosition(p);
         s.setColor(_defaultColor);
         s.setWidth(_defaultWidth);
         s.setHeight(_defaultHeight);

         _controlling.addShape(s);
     }

     public void draw()
     {
         _controlling.draw();
     }

     public void handleInput()
     {
         if (Input.mouseWasClicked(MouseButton.LEFT_BUTTON))
         {
             if (Input.isKeyPressed(Keys.VK_SHIFT))
             {
                 moveTo((Point)Input.getMousePosition().clone());
             }
             else
             {
                 addNewShape(Input.getMousePosition());
             }
         }
         
         if (Input.mouseWasClicked(MouseButton.RIGHT_BUTTON))
         {
             selectShape(Input.getMousePosition());
         }

         if (Input.wasKeyTyped(Keys.VK_R)) _adding = DrawingElements.RECTANGLE;
         if (Input.wasKeyTyped(Keys.VK_L)) _adding = DrawingElements.LINE;
         if (Input.wasKeyTyped(Keys.VK_E)) _adding = DrawingElements.ELLIPSE;

         if (Input.isKeyPressed(Keys.VK_SHIFT))
         {
             if (Input.isKeyPressed(Keys.VK_LEFT)) changeSize(-1, 0);
             if (Input.isKeyPressed(Keys.VK_RIGHT)) changeSize(1, 0);
             if (Input.isKeyPressed(Keys.VK_UP)) changeSize(0, -1);
             if (Input.isKeyPressed(Keys.VK_DOWN)) changeSize(0, 1);
         }
         else
         {
             if (Input.isKeyPressed(Keys.VK_LEFT)) move(-1, 0);
             if (Input.isKeyPressed(Keys.VK_RIGHT)) move(1, 0);
             if (Input.isKeyPressed(Keys.VK_UP)) move(0, -1);
             if (Input.isKeyPressed(Keys.VK_DOWN)) move(0, 1);
         }

         if (Input.isKeyPressed(Keys.VK_C)) changeColor();
     }
 }

