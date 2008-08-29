package swingame.emulator;

import java.awt.Image;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Frame;
import java.awt.Insets;
import java.awt.Point;
import java.awt.image.BufferedImage;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.KeyAdapter;

import swingame.MouseButton;
import swingame.Keys;

/**
 * <<Class summary>>
 *
 * @author   &lt;&gt;
 * @version $Rev$
 */
public final class EmulatedWindow extends Frame
{
	static final long serialVersionUID = -2221879713450231845L;
	
    private Point _mouse = new Point(0,0);
    private Image _backImage;
    private Image _nextImage;
    
    public EmulatedWindow(String caption, int width, int height) 
    {
        Insets insets = getInsets();
        
        setTitle(caption);
        setResizable(false);
        
        int totalWidth = width + (insets.left + insets.right);
        int totalHeight = height + (insets.top + insets.bottom);
        setSize(totalWidth, totalHeight);
        
        _backImage = new BufferedImage(width, height, BufferedImage.TYPE_4BYTE_ABGR);
        _nextImage = new BufferedImage(width, height, BufferedImage.TYPE_4BYTE_ABGR);
                
        addWindowListener ( new  WindowAdapter() {
                public void windowClosing(WindowEvent e) 
                {
                    System.exit(0);
                }
            });
        addMouseMotionListener( new MouseMotionAdapter() {
                public void mouseMoved(MouseEvent e)
                {
                    setMousePoint(e.getPoint());
                }
                public void mouseDragged(MouseEvent e)
                {
                    setMousePoint(e.getPoint());
                }
            });
        addMouseListener( new MouseAdapter() {
                public void mouseClicked(MouseEvent e)
                {
                    System.out.println("Click " + mapButton(e.getButton()) + " " + MouseButton.RIGHT_BUTTON);
                    _next.buttonClicked[mapButton(e.getButton()) - 1] = true;
                }
            });
        addKeyListener( new KeyAdapter(){
                public void keyPressed(KeyEvent e)
                {
                    _next.keyState[mapKey(e.getKeyCode())] |= EventStateData.KEY_DOWN;
                    //System.out.println("Type " + e.getKeyCode() + " " + Keys.VK_SPACE + " " + _next.keyState[mapKey(Keys.VK_SPACE)]);
                }
                public void keyReleased(KeyEvent e)
                {
                    _next.keyState[mapKey(e.getKeyCode())] = EventStateData.KEY_PRESSED;
                    //System.out.println("Type " + e.getKeyCode() + " " + Keys.VK_SPACE + " " + _next.keyState[mapKey(Keys.VK_SPACE)]);
                }
            });
    }
    
    private void setMousePoint(Point p)
    {
        Insets insets = getInsets();
        _mouse.setLocation(p.getX() - insets.left, p.getY() - insets.top);
    }
    
    protected Graphics getScreenGraphics()
    {
        return _nextImage.getGraphics();
    }
    
    protected Point getMousePoint()
    {
        return _mouse;
    }
    
    public void refreshScreen()
    {
        Graphics g = _backImage.getGraphics();
        g.drawImage(_nextImage, 0, 0, null);
        repaint();
    }
    
    public void paint(Graphics g)
    {
        super.paint(g);
        Insets insets = getInsets();        
        g.drawImage(_backImage, insets.left, insets.top, null);
    }
    
    private class EventStateData
    {
        public static final int KEY_DOWN = 1;
        public static final int KEY_PRESSED = 2;
        
        public boolean[] buttonClicked;
        //public boolean[] buttonDown;
        public int[] keyState;
        
        public EventStateData()
        {
            //7 mouse buttons
            buttonClicked = new boolean[7];
            //buttonDown = new boolean[7];
            
            keyState = new int[320];
        }
    }
    
    private EventStateData _current = new EventStateData();
    private EventStateData _next = new EventStateData();
    
    protected void processEvents()
    {
        //EventStateData old = _current;
        _current = _next;
        _next = new EventStateData();
        
        for(int i = 0; i < _current.keyState.length; i++)
        {
            //preserve key down
            _next.keyState[i] = _current.keyState[i] & EventStateData.KEY_DOWN;
        }
    }
    
    public void update(Graphics g)
    {
        paint(g);
    }
    
    protected boolean mouseWasClicked(int btn)
    {
        return _current.buttonClicked[btn - 1];
    }
    
    protected boolean keyTyped(int key)
    {
        return (_current.keyState[key] & EventStateData.KEY_PRESSED) == EventStateData.KEY_PRESSED;
    }

    protected boolean keyDown(int key)
    {
        return (_current.keyState[key] & EventStateData.KEY_DOWN) == EventStateData.KEY_DOWN;
    }
    
    private int mapButton(int awtButton)
    {
        switch(awtButton)
        {
            case MouseEvent.BUTTON1: return MouseButton.LEFT_BUTTON;
            case MouseEvent.BUTTON3: return MouseButton.RIGHT_BUTTON;
            case MouseEvent.BUTTON2: return MouseButton.MIDDLE_BUTTON;
        }
        return MouseButton.X1_BUTTON;
    }
    
    private int mapKey(int awtKey)
    {
        switch(awtKey)
        {
            case KeyEvent.VK_BACK_SPACE : return  Keys.VK_BACK;
            case KeyEvent.VK_TAB        : return  Keys.VK_TAB;
            case KeyEvent.VK_CLEAR      : return  Keys.VK_CLEAR;
            case KeyEvent.VK_ENTER     : return  Keys.VK_RETURN;
            case KeyEvent.VK_SHIFT      : return  Keys.VK_SHIFT;
            case KeyEvent.VK_CONTROL    : return  Keys.VK_CONTROL;
            case KeyEvent.VK_META       : return  Keys.VK_MENU;
            case KeyEvent.VK_ALT        : return  Keys.VK_ALT;
            case KeyEvent.VK_PAUSE  : return  Keys.VK_PAUSE;
            //case KeyEvent.VK_CAPITAL  : return  Keys.VK_CAPITAL;
            case KeyEvent.VK_ESCAPE  : return  Keys.VK_ESCAPE;
            case KeyEvent.VK_SPACE  : return  Keys.VK_SPACE;
            case KeyEvent.VK_PAGE_UP  : return  Keys.VK_PAGE_UP;
            case KeyEvent.VK_PAGE_DOWN  : return  Keys.VK_PAGE_DOWN;
            case KeyEvent.VK_END  : return  Keys.VK_END;
            case KeyEvent.VK_HOME  : return  Keys.VK_HOME;
            case KeyEvent.VK_LEFT  : return  Keys.VK_LEFT;
            case KeyEvent.VK_UP  : return  Keys.VK_UP;
            case KeyEvent.VK_RIGHT  : return  Keys.VK_RIGHT;
            case KeyEvent.VK_DOWN  : return  Keys.VK_DOWN;
            //case KeyEvent.VK_PRINT  : return  Keys.VK_PRINT;
            case KeyEvent.VK_INSERT  : return  Keys.VK_INSERT;
            case KeyEvent.VK_DELETE  : return  Keys.VK_DELETE;
            case KeyEvent.VK_HELP  : return  Keys.VK_HELP;
            case KeyEvent.VK_0  : return  Keys.VK_0;
            case KeyEvent.VK_1  : return  Keys.VK_1;
            case KeyEvent.VK_2  : return  Keys.VK_2;
            case KeyEvent.VK_3  : return  Keys.VK_3;
            case KeyEvent.VK_4  : return  Keys.VK_4;
            case KeyEvent.VK_5  : return  Keys.VK_5;
            case KeyEvent.VK_6  : return  Keys.VK_6;
            case KeyEvent.VK_7: return Keys.VK_7;
            case KeyEvent.VK_8: return Keys.VK_8;
            case KeyEvent.VK_9: return Keys.VK_9;
            case KeyEvent.VK_A: return Keys.VK_A;
            case KeyEvent.VK_B: return Keys.VK_B;
            case KeyEvent.VK_C: return Keys.VK_C;
            case KeyEvent.VK_D: return Keys.VK_D;
            case KeyEvent.VK_E: return Keys.VK_E;
            case KeyEvent.VK_F: return Keys.VK_F;
            case KeyEvent.VK_G: return Keys.VK_G;
            case KeyEvent.VK_H: return Keys.VK_H;
            case KeyEvent.VK_I: return Keys.VK_I;
            case KeyEvent.VK_J: return Keys.VK_J;
            case KeyEvent.VK_K: return Keys.VK_K;
            case KeyEvent.VK_L: return Keys.VK_L;
            case KeyEvent.VK_M: return Keys.VK_M;
            case KeyEvent.VK_N: return Keys.VK_N;
            case KeyEvent.VK_O: return Keys.VK_O;
            case KeyEvent.VK_P: return Keys.VK_P;
            case KeyEvent.VK_Q: return Keys.VK_Q;
            case KeyEvent.VK_R: return Keys.VK_R;
            case KeyEvent.VK_S: return Keys.VK_S;
            case KeyEvent.VK_T: return Keys.VK_T;
            case KeyEvent.VK_U: return Keys.VK_U;
            case KeyEvent.VK_V: return Keys.VK_V;
            case KeyEvent.VK_W: return Keys.VK_W;
            case KeyEvent.VK_X: return Keys.VK_X;
            case KeyEvent.VK_Y: return Keys.VK_Y;
            case KeyEvent.VK_Z: return Keys.VK_Z;
            //case KeyEvent.VK_LWIN: return Keys.VK_LWIN;
            //case KeyEvent.VK_RWIN: return Keys.VK_RWIN;
            //case KeyEvent.VK_APPS: return Keys.VK_APPS;
            //case KeyEvent.VK_SLEEP: return Keys.VK_SLEEP;
            case KeyEvent.VK_NUMPAD0: return Keys.VK_NUMPAD0;
            case KeyEvent.VK_NUMPAD1: return Keys.VK_NUMPAD1;
            case KeyEvent.VK_NUMPAD2: return Keys.VK_NUMPAD2;
            case KeyEvent.VK_NUMPAD3: return Keys.VK_NUMPAD3;
            case KeyEvent.VK_NUMPAD4: return Keys.VK_NUMPAD4;
            case KeyEvent.VK_NUMPAD5: return Keys.VK_NUMPAD5;
            case KeyEvent.VK_NUMPAD6: return Keys.VK_NUMPAD6;
            case KeyEvent.VK_NUMPAD7: return Keys.VK_NUMPAD7;
            case KeyEvent.VK_NUMPAD8: return Keys.VK_NUMPAD8;
            case KeyEvent.VK_NUMPAD9: return Keys.VK_NUMPAD9;
            case KeyEvent.VK_MULTIPLY: return Keys.VK_MULTIPLY;
            case KeyEvent.VK_ADD: return Keys.VK_ADD;
            case KeyEvent.VK_SUBTRACT: return Keys.VK_SUBTRACT;
            case KeyEvent.VK_DECIMAL: return Keys.VK_DECIMAL;
            case KeyEvent.VK_DIVIDE: return Keys.VK_DIVIDE;
            case KeyEvent.VK_F1: return Keys.VK_F1;
            case KeyEvent.VK_F2: return Keys.VK_F2;
            case KeyEvent.VK_F3: return Keys.VK_F3;
            case KeyEvent.VK_F4: return Keys.VK_F4;
            case KeyEvent.VK_F5: return Keys.VK_F5;
            case KeyEvent.VK_F6: return Keys.VK_F6;
            case KeyEvent.VK_F7: return Keys.VK_F7;
            case KeyEvent.VK_F8: return Keys.VK_F8;
            case KeyEvent.VK_F9: return Keys.VK_F9;
            case KeyEvent.VK_F10: return Keys.VK_F10;
            case KeyEvent.VK_F11: return Keys.VK_F11;
            case KeyEvent.VK_F12: return Keys.VK_F12;
            case KeyEvent.VK_F13: return Keys.VK_F13;
            case KeyEvent.VK_F14: return Keys.VK_F14;
            case KeyEvent.VK_F15: return Keys.VK_F15;
            //case KeyEvent.VK_NUMLOCK: return Keys.VK_NUMLOCK;
            //case KeyEvent.VK_SCROLL: return Keys.VK_SCROLL;
            //case KeyEvent.VK_LSHIFT: return Keys.VK_LSHIFT;
            //case KeyEvent.VK_RSHIFT: return Keys.VK_RSHIFT;
            //case KeyEvent.VK_LCONTROL: return Keys.VK_LCONTROL;
            //case KeyEvent.VK_RCONTROL: return Keys.VK_RCONTROL;
            //case KeyEvent.VK_LMENU: return Keys.VK_LMENU;
            //case KeyEvent.VK_LALT: return Keys.VK_LALT;
            //case KeyEvent.VK_RMENU: return Keys.VK_RMENU;
            //case KeyEvent.VK_RALT: return Keys.VK_RALT;
            case KeyEvent.VK_EQUALS: return Keys.VK_EQUALS;
            case KeyEvent.VK_COLON: return Keys.VK_COLON;
            case KeyEvent.VK_SEMICOLON: return Keys.VK_SEMICOLON;
            case KeyEvent.VK_LESS: return Keys.VK_LESS;
            case KeyEvent.VK_GREATER: return Keys.VK_GREATER;
            //case KeyEvent.VK_QUESTION: return Keys.VK_QUESTION;
            case KeyEvent.VK_AT: return Keys.VK_AT;
            case KeyEvent.VK_COMMA: return Keys.VK_COMMA;
            case KeyEvent.VK_PERIOD: return Keys.VK_PERIOD;
            case KeyEvent.VK_SLASH: return Keys.VK_SLASH;
        }
        
        return Keys.VK_NUMLOCK;
    }
}
