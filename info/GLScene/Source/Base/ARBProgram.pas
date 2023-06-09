//
// This unit is part of the GLScene Project, http://glscene.org
//
{: ARBProgram<p>

   Some useful methods for setting up ARB vertex and fragment programs.<p>

   <b>History : </b><font size=-1><ul>
      <li>04/06/10 - Yar - Replace OpenGL functons to OpenGLAdapter
      <li>31/03/07 - DaStr - Added $I GLScene.inc
      <li>11/10/04 - SG - Creation
   </ul></font>
}
unit ARBProgram;

interface

{$I GLScene.inc}

uses
   SysUtils, OpenGL1x, GLContext;

procedure LoadARBProgram(target : GLenum; programText : String; var handle : cardinal);

implementation

procedure LoadARBProgram(target : GLenum; programText : String; var handle : cardinal);
var
   errPos : Integer;
   errString : String;
begin
   if (target = GL_VERTEX_PROGRAM_ARB) and not GL.ARB_vertex_program then
      raise Exception.Create('GL_ARB_vertex_program required!');
   if (target = GL_FRAGMENT_PROGRAM_ARB) and not GL.ARB_fragment_program then
      raise Exception.Create('GL_ARB_fragment_program required!');
   GL.GenPrograms(1, @handle);
   GL.BindProgram(target, handle);
   GL.ProgramString(target, GL_PROGRAM_FORMAT_ASCII_ARB,
      Length(programText), PGLChar(TGLString(programText)));
   GL.GetIntegerv(GL_PROGRAM_ERROR_POSITION_ARB, @errPos);
   if errPos>-1 then begin
      errString:=String(glGetString(GL_PROGRAM_ERROR_STRING_ARB));
      raise Exception.CreateFmt('ARB Program Error - [Handle: %d][Pos: %d][Error %s]', [handle, errPos, errString]);
   end;
   GL.CheckError;
end;

end.
