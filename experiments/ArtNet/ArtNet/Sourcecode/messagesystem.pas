unit messagesystem;

{
The PC_DIMMER messagesystem lets you interact with the mainprogram. You can
set, record, receive and control values with the predefined messages.

Use the pointer of the callbackfunction "CallbackSendMessage" to send the
messages like this:

  // in your program:
  TCallbackSendMessage = procedure(MSG: Byte; ARG:Variant);stdcall;
  SendMSG:TCallbackSendMessage;

  // in the DLLActivate-prodecure of the DPR-file:
  @YOURPROGRAM.SendMSG:=CallbackSendMessage;

  // back in your program:
  SendMSG(MSG_ADDLOGFILEENTRY,'The messagesystem is working well! :)'); // Adds a new entry into the debug-log
}

interface

const
	// PC_DIMMER Messagesystem: MSG, Data1, Data2
	MSG_RECORDSCENE=0; // Booleanarray
	MSG_AUDIOEFFECTPLAYERRECORD=1; // Boolean
	MSG_SYSTEMVOLUME=2; // Byte
	MSG_SYSTEMMUTE=3; // Boolean
	MSG_JUMPTOCHANNEL=4; // Integer
	MSG_GRANDMASTER=5; // Byte
	MSG_FLASHMASTER=6; // Byte
	MSG_ADDLOGFILEENTRY=7; // String
	MSG_EFFECTSPLAY=8; // Boolean
  MSG_SYSTEMSPEED=9; // Integer und Float (BPM)
  MSG_NEW=10; // String (Projektverzeichnis)
  MSG_OPEN=11; // String (Projektverzeichnis)
  MSG_SAVE=12; // String (Projektverzeichnis)
  MSG_BEATIMPULSE=13; // Boolean
  MSG_ACTUALCHANNELVALUE=14; // Integer, Integer
  MSG_MIDIIN=15; // array[0..2] of byte
  MSG_STARTSCENE=16; // TGUID
  MSG_STOPSCENE=17; // TGUID
  MSG_SPEEDMASTER=18; // Byte

implementation

end.