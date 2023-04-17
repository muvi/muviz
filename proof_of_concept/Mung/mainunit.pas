unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, dglOpenGL, GLContext, GLPlatformContext, GLType;

type

  { TForm1 }

  TForm1    = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ContextPanel: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FContext: TGLContext;
    //Test
    cube_rotationx: GLFloat;
    cube_rotationy: GLFloat;
    cube_rotationz: GLFloat;
  end;

  ETestError= class (Exception)
  public
    constructor Create(Msg: string);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FContext.Destroy;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  Speed: Double;
begin

  glClearColor(0.0, 0.0, 0.0, 1.0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glEnable(GL_DEPTH_TEST);

  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_POINT_SMOOTH);
  glEnable(GL_LINE_SMOOTH);

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(45.0, double(ContextPanel.Width) / ContextPanel.Height, 0.1, 100.0);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

  glTranslatef(0.0, 0.0,-6.0);
  glRotatef(cube_rotationx, cube_rotationy, cube_rotationz, 0.0);

  glEnable(GL_TEXTURE_2D);

  //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);


  glBegin(GL_QUADS);
          glColor3f(0.0,1.0,0.0);                              // Set The Color To Green
          glVertex3f( 1.0, 1.0,-1.0);                  // Top Right Of The Quad (Top)
          glVertex3f(-1.0, 1.0,-1.0);                  // Top Left Of The Quad (Top)
          glVertex3f(-1.0, 1.0, 1.0);                  // Bottom Left Of The Quad (Top)
          glVertex3f( 1.0, 1.0, 1.0);                  // Bottom Right Of The Quad (Top)
  glEnd();
  glBegin(GL_QUADS);
          glColor3f(1.0,0.5,0.0);                              // Set The Color To Orange
          glVertex3f( 1.0,-1.0, 1.0);                  // Top Right Of The Quad (Bottom)
          glVertex3f(-1.0,-1.0, 1.0);                  // Top Left Of The Quad (Bottom)
          glVertex3f(-1.0,-1.0,-1.0);                  // Bottom Left Of The Quad (Bottom)
          glVertex3f( 1.0,-1.0,-1.0);                  // Bottom Right Of The Quad (Bottom)
  glEnd();
  glBegin(GL_QUADS);
          glColor3f(1.0,0.0,0.0);                              // Set The Color To Red
          glVertex3f( 1.0, 1.0, 1.0);                  // Top Right Of The Quad (Front)
          glVertex3f(-1.0, 1.0, 1.0);                  // Top Left Of The Quad (Front)
          glColor3f(1.0,1.0,0.0);                              // Set The Color To Red
          glVertex3f(-1.0,-1.0, 1.0);                  // Bottom Left Of The Quad (Front)
          glVertex3f( 1.0,-1.0, 1.0);                  // Bottom Right Of The Quad (Front)
  glEnd();
  glBegin(GL_QUADS);
          glColor3f(1.0,1.0,0.0);                              // Set The Color To Yellow
          glVertex3f( 1.0,-1.0,-1.0);                  // Bottom Left Of The Quad (Back)
          glVertex3f(-1.0,-1.0,-1.0);                  // Bottom Right Of The Quad (Back)
          glVertex3f(-1.0, 1.0,-1.0);                  // Top Right Of The Quad (Back)
          glVertex3f( 1.0, 1.0,-1.0);                  // Top Left Of The Quad (Back)
  glEnd();
  glBegin(GL_QUADS);
          glColor3f(0.0,0.0,1.0);                              // Set The Color To Blue
          glVertex3f(-1.0, 1.0, 1.0);                  // Top Right Of The Quad (Left)
          glVertex3f(-1.0, 1.0,-1.0);                  // Top Left Of The Quad (Left)
          glVertex3f(-1.0,-1.0,-1.0);                  // Bottom Left Of The Quad (Left)
          glVertex3f(-1.0,-1.0, 1.0);                  // Bottom Right Of The Quad (Left)
  glEnd();
  glBegin(GL_QUADS);
          glColor4f(1.0,0.0,1.0,0.5);                              // Set The Color To Violet
          glVertex3f( 1.0, 1.0,-1.0);                  // Top Right Of The Quad (Right)
          glVertex3f( 1.0, 1.0, 1.0);                  // Top Left Of The Quad (Right)
          glVertex3f( 1.0,-1.0, 1.0);                  // Bottom Left Of The Quad (Right)
          glVertex3f( 1.0,-1.0,-1.0);                  // Bottom Right Of The Quad (Right)
  glEnd();
  glLineWidth(3);
  glBegin(GL_LINES);
          glColor4f(0.0,0.0,1.0,1.0);                              // Set The Color To Violet
          glVertex3f( 2.0, 1.0,-1.0);                  // Top Right Of The Quad (Right)
          glVertex3f( 2.0, 1.0, 1.0);                  // Top Left Of The Quad (Right)
  glEnd();

  //Speed := double(OpenGLControl1.FrameDiffTimeInMSecs)/10;
  Speed:=4;

  cube_rotationx += 5.15 * Speed;
  cube_rotationy += 5.15 * Speed;
  cube_rotationz += 20.0 * Speed;

  FContext.SwapBuffers;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  VertexArrayID: GLuint;
  vertexbuffer : GLuint;

  g_vertex_buffer_data: array [0..8] of GLFloat = (
    -1.0, -1.0, 0.0,
    1.0,  -1.0, 0.0,
    0.0,  1.0,  0.0
  );
begin
  glClearColor(0.0, 0.0, 0.0, 1.0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  glGenVertexArrays(1,@VertexArrayID);
  glBindVertexArray(VertexArrayID);

  glGenBuffers(1,@vertexbuffer);
  glBindBuffer(GL_ARRAY_BUFFER,vertexbuffer);
  // Give our vertices to OpenGL.
  glBufferData(GL_ARRAY_BUFFER,SizeOf(g_vertex_buffer_data),@g_vertex_buffer_data,GL_STATIC_DRAW);

  //raise ETestError.Create('VertexArrayID: '+IntToStr(VertexArrayID)+'; vertexbuffer: '+IntToStr(vertexbuffer));

  glEnableVertexAttribArray(0);
  //if glGetError()=GL_NO_ERROR then raise ETestError.Create('Gl Error');

  glBindBuffer(GL_ARRAY_BUFFER,vertexbuffer);
  glVertexAttribPointer(
     0,                  // attribute 0. No particular reason for 0, but must match the layout in the shader.
     3,                  // size
     GL_FLOAT,           // type
     false,           // normalized?
     0,                  // stride
     nil                 // array buffer offset
  );
  glDrawArrays(GL_TRIANGLES, 0, 3); // Starting from vertex 0; 3 vertices total -> 1 triangle
  glDisableVertexAttribArray(0);

  if glGetError()<>GL_NO_ERROR then raise ETestError.Create('error');

  FContext.SwapBuffers;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  cube_rotationx:=0;
  cube_rotationy:=0;
  cube_rotationz:=0;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  InitOpenGL;
  ReadExtensions;
  ReadImplementationProperties;
  FContext:=TGLCrossContext.Create(ContextPanel.Handle,3,3);
  //ReadExtensions;
  //ReadImplementationProperties;
  FContext.Activate;
end;

{ETestError}

constructor ETestError.Create(Msg: string);
begin
  inherited Create(Msg);
end;

end.

