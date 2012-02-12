program OpenGLTextTest;
uses
  sgDriverGraphics, sgGraphics, sgUtils,sgInput, 
  math, gl, glext, glu;
  
// All code copyright to Ryan Pridgeon, 2009
// If you wish to use this code, you must include these credits
// in your code, and give credit in your application/documentation
// Thankyou
//

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


type
    FT_Int      = longint;
    FT_Int16    = smallint;
    FT_UInt16   = word;
    FT_Int32    = longint;
    FT_UInt32   = dword;
    FT_Fast     = longint;
    FT_UFast    = dword;
    FT_INT64    = int64;
    FT_Bytes    = ^FT_Byte;
    size_t      = PtrInt;

    FT_Bool     = byte;
    FT_FWord    = smallint;
    FT_UFWord   = word;
    FT_Char     = char;
    FT_Byte     = byte;
    FT_Tag      = FT_UInt32;
    FT_String   = char;
    FT_Short    = smallint;
    FT_UShort   = word;
    FT_UInt     = dword;
    FT_ULong    = dword;
    FT_F2Dot14  = smallint;
    FT_F26Dot6  = longint;
    FT_Fixed    = longint;
    FT_Error    = longint;
    FT_Pointer  = pointer;
    FT_Offset   = size_t;
    //FT_PtrDist  = ft_ptrdiff_t;
    
    FT_UnitVector = record
      x : FT_F2Dot14;
      y : FT_F2Dot14;
    end;

    FT_Matrix = record
      xx : FT_Fixed;
      xy : FT_Fixed;
      yx : FT_Fixed;
      yy : FT_Fixed;
    end;

    FT_Data = record
      pointer : ^FT_Byte;
      length : FT_Int;
    end;

    FT_Generic_Finalizer = procedure (aobject: pointer);cdecl;

    FT_Generic = record
      data      : pointer;
      finalizer : FT_Generic_Finalizer;
    end;

    // function FT_MAKE_TAG(_x1,_x2,_x3,_x4 : longint) : FT_Tag; 

    FT_ListNode = ^FT_ListNodeRec;
    FT_List = ^FT_ListRec;

    FT_ListNodeRec = record
      prev : FT_ListNode;
      next : FT_ListNode;
      data : pointer;
    end;

    FT_ListRec = record
      head : FT_ListNode;
      tail : FT_ListNode;
    end;

    // function FT_IS_EMPTY(list : longint) : longint;




	// FT_Int32	= LongInt;
    FT_Long 	= LongInt;
 //    FT_Error 	= LongInt;
 //    FT_UInt 	= LongWord;
 //    FT_F26Dot6 	= LongWord;
 //    FT_ULong	= LongWord;
 //    FT_Pointer	= Pointer;
 //    FT_Bool		= Byte;
	// FT_Int 		= LongInt; //check
	FT_Pos 		= LongInt; //signed long
	// PByte 		= ^Byte;
	// FT_UShort 	= Word;
	// FT_Short 	= SmallInt;

	FT_Render_Mode = (
    	FT_RENDER_MODE_NORMAL = 0,
    	FT_RENDER_MODE_LIGHT,
    	FT_RENDER_MODE_MONO,
    	FT_RENDER_MODE_LCD,
    	FT_RENDER_MODE_LCD_V,

    	FT_RENDER_MODE_MAX
    	);

	FT_Library = 	Pointer;
    FT_Face = 		^FT_FaceRec;
    FT_Glyph = 		^FT_GlyphRec;
    FT_GlyphSlot = 	^FT_GlyphSlotRec;
    FT_Size = 		Pointer;
    FT_SubGlyph =   Pointer;
    FT_Slot_Internal = Pointer;

	FT_Vector = record
	    x: FT_Pos;
	    y: FT_Pos;
	end;
	PFT_Vector = ^FT_Vector;

	FT_BBox = record
	    xMin, yMin: FT_Pos;
	    xMax, yMax: FT_Pos;
    end;

	FT_Bitmap = record
    	rows: 			LongInt;
    	width: 			LongInt;
    	pitch: 			LongInt;
    	buffer: 		PByte;
    	num_grays: 		SmallInt; //short
    	pixel_mode: 	Char;
    	palette_mode: 	Char;
    	palette: 		Pointer;
  	end;
  	PFT_Bitmap = ^FT_Bitmap;

	GLFontData = record
    	textures: 		array [0..127] of GLuint ;
    	displaylist: 	GLuint ;
    	h: 				Single;
    end;

	FT_GlyphRec = record
	    alibrary: 	FT_Library;
	    clazz:		Pointer; //PFT_Glyph_Class;
	    format:		LongInt; //FT_Glyph_Format;
	    advance:	FT_Vector;
	end;

    FT_BitmapGlyphRec = record
	    root: 	FT_GlyphRec;
	    left: 	FT_Int;
	    top: 	FT_Int;
	    bitmap: FT_Bitmap;
	end;

	// FT_Generic = record
	// 	data: Pointer;
 //      	finalizer: Pointer; //FT_Generic_Finalizer;
	// end;

	FT_CharMap = Pointer;

	// FT_ListRec = record
 //    	head, tail: Pointer;
 //    end;

	FT_FaceRec = record
    	num_faces			: FT_Long;
    	face_index			: FT_Long;

    	face_flags			: FT_Long;
    	style_flags			: FT_Long;

    	num_glyphs			: FT_Long;

    	family_name			: Pointer; //FT_String*;
    	style_name			: Pointer; //FT_String*;

    	num_fixed_sizes		: FT_Int;
    	available_sizes		: Pointer; //FT_Bitmap_Size* ;

    	num_charmaps		: FT_Int;
    	charmaps			: Pointer; //FT_CharMap* ;

    	generic				:FT_Generic;

	    //*# The following member variables (down to `underline_thickness') */
	    //*# are only relevant to scalable outlines; cf. @FT_Bitmap_Size    */
	    //*# for bitmap fonts.                                              */
    	bbox :				FT_BBox;

    	units_per_EM :		FT_UShort;
    	ascender :			FT_Short;
    	descender :			FT_Short;
    	height :			FT_Short;

    	max_advance_width:	FT_Short;
    	max_advance_height:	FT_Short;

    	underline_position:	FT_Short;
    	underline_thickness:FT_Short;

    	glyph:				FT_GlyphSlot;
    	size:				FT_Size;
    	charmap:			FT_CharMap;

    	//*@private begin */

    	driver:				Pointer; //FT_Driver;
    	memory:				Pointer; //FT_Memory;
    	stream:				Pointer; //FT_Stream;

    	sizes_list:			FT_ListRec;

    	autohint:			FT_Generic;
    	extensions:			Pointer;

    	internal:			Pointer; //FT_Face_Internal;
    	//*@private end */
  end;

  FT_Glyph_Metrics = record
    width           : FT_Pos;
    height          : FT_Pos;
 
    horiBearingX    : FT_Pos;
    horiBearingY    : FT_Pos;
    horiAdvance     : FT_Pos;
 
    vertBearingX    : FT_Pos;
    vertBearingY    : FT_Pos;
    vertAdvance     : FT_Pos;
  end;

  FT_Outline = record
    n_contours: word; // number of contours in glyph        */
    n_points:   word;   // number of points in the glyph      */

    points:     PFT_Vector;     // the outline's points
    tags:       PChar;       // the points flags                   */
    contours:   ^word;   // the contour end points             */

    flags:      LongInt;      // outline masks                      */

  end;

  FT_GlyphSlotRec = record
    alibrary:           FT_Library;
    face:               FT_Face;
    next:               FT_GlyphSlot;
    reserved:           FT_UInt;       // retained for binary compatibility
    generic:            FT_Generic;

    metrics:            FT_Glyph_Metrics;
    linearHoriAdvance:  FT_Fixed;
    linearVertAdvance:  FT_Fixed;
    advance:            FT_Vector;

    format:             longint; //FT_Glyph_Format;

    bitmap:             FT_Bitmap;
    bitmap_left:        FT_Int;
    bitmap_top:         FT_Int;

    outline:            FT_Outline;

    num_subglyphs:      FT_UInt;
    subglyphs:          FT_SubGlyph;

    control_data:       Pointer;
    control_len:        longint;

    lsb_delta:          FT_Pos;
    rsb_delta:          FT_Pos;

    other:              Pointer;

    internal:           FT_Slot_Internal;
  end;
	// record  FT_Glyph_Class
	//    glyph_size:  	FT_Long;
	//    glyph_format:  	FT_Glyph_Format;
	//    glyph_init:  	FT_Glyph_InitFunc;
	//    glyph_done:  	FT_Glyph_DoneFunc;
	//    glyph_copy:  	FT_Glyph_CopyFunc;
	//    glyph_transform: FT_Glyph_TransformFunc;
	//    glyph_bbox:  	FT_Glyph_GetBBoxFunc;
	//    glyph_prepare:  	FT_Glyph_PrepareFunc;
	// end;


    GLFont = ^GLFontData;

    PFT_Library = ^FT_Library;
    PFT_Face = ^FT_Face;
    PFT_Glyph = ^FT_Glyph;
    
    FT_BitmapGlyph = ^FT_BitmapGlyphRec;
    PFT_BitmapGlyph = ^FT_BitmapGlyph;
    

const FT_TRUE	= 1;
const FT_FALSE	= 0;

const FT_LOAD_DEFAULT                      = $0;
const FT_LOAD_NO_SCALE                     = $1;
const FT_LOAD_NO_HINTING                   = $2;
const FT_LOAD_RENDER                       = $4;
const FT_LOAD_NO_BITMAP                    = $8;
const FT_LOAD_VERTICAL_LAYOUT              = $10;
const FT_LOAD_FORCE_AUTOHINT               = $20;
const FT_LOAD_CROP_BITMAP                  = $40;
const FT_LOAD_PEDANTIC                     = $80;
const FT_LOAD_IGNORE_GLOBAL_ADVANCE_WIDTH  = $200;
const FT_LOAD_NO_RECURSE                   = $400;
const FT_LOAD_IGNORE_TRANSFORM             = $800;
const FT_LOAD_MONOCHROME                   = $1000;
const FT_LOAD_LINEAR_DESIGN                = $2000;
const FT_LOAD_NO_AUTOHINT                  = $8000;


function FT_Init_FreeType( alibrary: PFT_Library ): FT_Error; cdecl; external;
function FT_Done_FreeType( alibrary: PFT_Library ): FT_Error; cdecl; external;

function FT_New_Face( alibrary: FT_Library; filepathname: PChar; face_index: FT_Long; aface: PFT_Face ): FT_Error; cdecl; external;
function FT_Done_Face(face: FT_Face): FT_Error; cdecl; external;

function FT_Set_Char_Size( 	face: FT_Face;
                    		char_width: FT_F26Dot6;
                    		char_height: FT_F26Dot6;
                         	horz_resolution: FT_UInt;
                    		vert_resolution: FT_UInt ): FT_Error; cdecl; external;

function FT_Load_Glyph( face: 			FT_Face;
                 		glyph_index: 	FT_UInt;
                 		load_flags:		FT_Int32 ): FT_Error; cdecl; external;

function FT_Get_Char_Index( face: 		FT_Face;
                     		charcode: 	FT_ULong ): FT_Error; cdecl; external;

function FT_Get_Glyph_Name(	face:  			FT_Face;
                     		glyph_index:	FT_UInt;
                     		buffer:			FT_Pointer;
                          	buffer_max:		FT_UInt ): FT_Error; cdecl; external;

function FT_Get_Glyph( 	slot: 		FT_GlyphSlot;
  	                  	aglyph: 	PFT_Glyph ): FT_Error; cdecl; external;

function FT_Glyph_To_Bitmap( 	the_glyph: 		PFT_Glyph;
                      			render_mode: 	FT_Render_Mode;
                      			origin: 		PFT_Vector;
                               	destroy:		FT_Bool ): FT_Error; cdecl; external;

function LoadFont(filepath: String; height: Integer) : GLFont;
var
    temp: 		GLFont;
    lib: 		FT_Library = nil;
    face: 		FT_Face = nil;
    glyph: 		FT_Glyph = nil;
    bmpGlyph:	FT_BitmapGlyph = nil;
    bitmap:  	PFT_Bitmap = nil;
    bitglyph: 	PFT_BitmapGlyph = nil;

    w, h, xpos, ypos: 	LongInt;
    i: 					LongInt;
    buffer:				array of GLUByte;
    texpropx, texpropy: Single;
begin
    New(temp);

WriteLn('here');
    glGenTextures(128, temp^.textures);
    WriteLn('here');
    temp^.displaylist := glGenLists(128);
    WriteLn('here');
    FT_Init_FreeType(@lib);
    FT_New_Face(lib, PChar(filepath), 0, @face);
    WriteLn(HexStr(face));
    FT_Set_Char_Size(face, height shl 6, height shl 6, 96, 96);

    for i := 0 to High(temp^.textures) do
    begin
    	// get bitmap
    	FT_Load_Glyph(face, FT_Get_Char_Index(face, i), FT_LOAD_DEFAULT);
    	FT_Get_Glyph(face^.glyph, @glyph);
    	FT_Glyph_To_Bitmap(@glyph, FT_RENDER_MODE_NORMAL, nil, FT_TRUE);
    	bmpGlyph := FT_BitmapGlyph(glyph);
    	bitglyph := @bmpGlyph;
    	bitmap := @(bmpGlyph^.bitmap);

        WriteLn(i, ' Advance ', face^.glyph^.advance.x, ':', face^.glyph^.advance.y);
        WriteLn('advance - glyph ', DWord(@(face^.glyph^.advance)) - dword(face^.glyph));
        WriteLn('glyph - face ', DWord(@(face^.glyph)) - dword(face));

    	w := 1;
    	while w < bitmap^.width do
    	begin
    		w *= 2;
    	end;

    	h := 1;
    	while h < bitmap^.rows do
    	begin
    		h *= 2;
    	end;

        // make bitmap
        WriteLn('w:', w, ' h:', h, ' ', 2*w*h);
        SetLength(buffer, 2*w*h); //buffer = (PGLubyte*)calloc(sizeof(GLubyte)*2*w*h, 1);
        
        for ypos := 0 to bitmap^.rows -1 do
        begin
            for xpos := 0 to bitmap^.width - 1 do
            begin
            	//Write(HexStr(bitmap^.buffer[ xpos + ypos * bitmap^.pitch], 2), ' ');

                buffer[2 * (xpos + ypos * w)] :=        $FF; //bitmap^.buffer[ xpos + ypos * bitmap^.pitch];
                buffer[2 * (xpos + ypos * w) + 1] :=    bitmap^.buffer[ xpos + ypos * bitmap^.pitch];
            end;
            // WriteLn();
        end;

        // WriteLn();
        // WriteLn((bitglyph^)^.top, ', ', face^.glyph^.advance.x, ':', face^.glyph^.advance.y);

        glBindTexture( GL_TEXTURE_2D, temp^.textures[i] );
        glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

        glTexImage2D( GL_TEXTURE_2D, 0, GL_RGBA, w, h, 0, GL_LUMINANCE_ALPHA, GL_UNSIGNED_BYTE, @buffer[0]);

        // make display list (for nicely formatted text)
        glPushMatrix();

        glNewList(temp^.displaylist+i, GL_COMPILE);
            glBindTexture(GL_TEXTURE_2D, temp^.textures[i]);
            glTranslatef((bitglyph^)^.left, (bitglyph^)^.top - bitmap^.rows, 0);

            glPushMatrix();
            glTranslatef((bitglyph^)^.left, (face^.height - face^.glyph^.metrics.horiBearingY) shr 6, 0);
            // proportions of the texture that are the font (not padding)
            texpropx := bitmap^.width / w;
            texpropy := bitmap^.rows / h;

            // glDrawPixels(bitmap^.width, bitmap^.rows, GL_LUMINANCE_ALPHA, GL_UNSIGNED_BYTE, @buffer[0]);
            glBegin(GL_QUADS);
             glTexCoord2f(0, 0);                glVertex3f(0, 0, 0);
             glTexCoord2f(0, texpropy);         glVertex3f(0, bitmap^.rows, 0);
             glTexCoord2f(texpropx, texpropy);  glVertex3f(bitmap^.width, bitmap^.rows, 0);
             glTexCoord2f(texpropx, 0);         glVertex3f(bitmap^.width, 0, 0);
            glEnd();

            glPopMatrix();

            glTranslatef(face^.glyph^.advance.x shr 6, face^.glyph^.advance.y shr 6, 0);
        glEndList();

        glPopMatrix();

        SetLength(buffer, 0);
    end;

    FT_Done_Face(face);
    FT_Done_FreeType(lib);

    result := temp;
end;

procedure PrintFont(temp: GLFont; text: String; x, y: Single);
begin
    glPushMatrix();
    glTranslatef(x, y, 0);

    glListBase(temp^.displaylist);
    glCallLists(Length(text), GL_UNSIGNED_BYTE, @text[1]);

    glPopMatrix();
end;

procedure FreeFont(var temp: GLFont);
begin
	if not assigned(temp) then exit;

    glDeleteTextures(128, temp^.textures);
    glDeleteLists(temp^.displaylist, 128);
    Dispose(temp);
    temp := nil;
end;


procedure InitGL;
// We call this right after our OpenGL window is created.
begin
  if not Load_GL_version_2_0() then
  begin
    WriteLn( 'Opengl 2.0 API not Supported');
    Halt();
  end;
  // Enable smooth shading 
  glShadeModel( GL_SMOOTH );

  // Set the background black 
  glClearColor( 0.0, 0.0, 0.0, 0.0 );

  // Depth buffer setup 
  glClearDepth( 1.0 );

  // Enables Depth Testing 
  glEnable( GL_DEPTH_TEST );

  // The Type Of Depth Test To Do 
  glDepthFunc( GL_LEQUAL );

  // Really Nice Perspective Calculations 
  glHint( GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST );

  // Load the basic shaders
  //LoadBasicShaderProgram;
end;

function ShaderGetInfoLog(s: GLuint): String;
var
  blen, slen: Integer;
  infolog: array of Char;
begin

  glGetShaderiv(s, GL_INFO_LOG_LENGTH, @blen);

  if blen > 1 then
  begin
    SetLength(infolog, blen);
    glGetShaderInfoLog(s, blen, @slen, @infolog[0]);
    Result := String(infolog);
    Exit;
  end;

  Result := '';

end;

procedure Main();
var
	f: GLFont;
    m:  array [0..15] of single;
    glsl_code, msg: String;
    myFragShader, myProg : GLuint;
    src:    PChar;
    len, compiled : glInt;
    temp: gluint;
    success: glInt;
begin
    glsl_code :=
        ' uniform sampler2D Tex_Clr0; '#10 +
        ' uniform sampler2D Tex_Alpha0; '#10 +
        ' void main(void) { //vec4 OutColor(0,0,0,0); '#10 +
        ' vec4 myColor = texture2D(Tex_Clr0,gl_TexCoord[0].xy); '#10 +
        ' //myColor.a = texture2D(Tex_Alpha0,gl_TexCoord[1].xy); '#10 +
        ' gl_FragColor = myColor - vec4(1.0, 0, 1.0, 0.0); '#10 +
        '} ';
    
    WriteLn('Face ', sizeof(FT_FaceRec));
    WriteLn('Glyph ', sizeof(FT_GlyphRec));

	Writeln('opening window');
	GraphicsDriver.InitializeGraphicsWindow('Hello World', 640, 480);
    InitGL();
	Writeln('clearing screen to red');
    glEnable (GL_BLEND);
    glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	
    ClearScreen(2147483647);
	Writeln('loading font');
    
    writeln('here');
    myFragShader := glCreateShader(GL_FRAGMENT_SHADER);
    // srcAddr := @(glsl_code[0]);
    len := Length(glsl_code);
    src := PChar(glsl_code);
    writeln('here');
    glShaderSource(myFragShader, 1, @src, @len);
    writeln('here');
    glCompileShader(myFragShader);
    writeln('here');
    glGetShaderiv(myFragShader, GL_COMPILE_STATUS, @compiled);
    msg := ShaderGetInfoLog(myFragShader);
    if compiled <> GL_TRUE then
    begin
      WriteLn('Error compiling shader : ', msg);
      exit;
    end;

    myProg := glCreateProgram();
    glAttachShader(myProg, myFragShader);
    glLinkProgram(myProg);
    glGetProgramIV(myProg, GL_LINK_STATUS, @success);
    if success = GL_FALSE then
    begin
        WriteLn('GL link failed');
        exit;
    end;

    glValidateProgram(myProg);
    glGetProgramIV(myProg, GL_VALIDATE_STATUS, @success);
    if success = GL_FALSE then
    begin
        WriteLn('GL Validate failed');
        exit;
    end;

    f := LoadFont('/Users/acain/working/SwinGameSDK/CoreSDK/test/Resources/fonts/arial.ttf', 18);


    glUseProgram(myProg);
	PrintFont(f, 'Hello World!', 50, 50);
    glUseProgram(0);

    PrintFont(f, 'Hello World!', 50, 100);

	RefreshScreen();
	Delay(5000);

	WriteLn('Loaded font...', HexStr(f));
	FreeFont(f);

	exit;	


	repeat
		ProcessEvents();
		ClearScreen(rnd(2147483647));
		RefreshScreen();
	until WindowCloseRequested();
end;

begin
	Main();
end.