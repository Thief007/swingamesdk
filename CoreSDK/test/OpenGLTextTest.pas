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


// procedure InitGL;
// // We call this right after our OpenGL window is created.
// begin
//   if not Load_GL_version_2_0() then
//   begin
//     WriteLn( 'Opengl 2.0 API not Supported');
//     Halt();
//   end;
//   // Enable smooth shading 
//   glShadeModel( GL_SMOOTH );

//   // Set the background black 
//   glClearColor( 0.0, 0.0, 0.0, 0.0 );

//   // Depth buffer setup 
//   glClearDepth( 1.0 );

//   // Enables Depth Testing 
//   glEnable( GL_DEPTH_TEST );

//   // The Type Of Depth Test To Do 
//   glDepthFunc( GL_LEQUAL );

//   // Really Nice Perspective Calculations 
//   glHint( GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST );

//   // Load the basic shaders
//   //LoadBasicShaderProgram;
// end;

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
        ' vec4 myColor = texture2D(Tex_Clr0,gl_TexCoord[0].xy) * gl_Color; '#10 +
        ' //myColor.a = texture2D(Tex_Alpha0,gl_TexCoord[1].xy); '#10 +
        ' gl_FragColor = myColor; '#10 +
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

    f := LoadFont('/Users/acain/Source/Google/SwinGameSDK/CoreSDK/test/Resources/fonts/arial.ttf', 18);
	WriteLn('Loaded font...', HexStr(f));


    glUseProgram(myProg);
    glColor4f ( 1.0, 0, 0, 1.0 );
	PrintFont(f, 'Hello World!', 50, 50);
    glUseProgram(0);

    PrintFont(f, 'Hello World!', 50, 100);

	RefreshScreen();
	Delay(5000);

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