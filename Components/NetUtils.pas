{ *********************************************************************** }
{                                                                         }
{ NetUtils                                                                }
{                                                                         }
{ Copyright (c) 2013 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit NetUtils;

interface

uses
  Windows, Classes, Winsock;

{ ��������� ������ �� ��������� C ������ }

const
  SIO_GET_INTERFACE_LIST = $4004747F;
  IFF_UP = $00000001;
  IFF_BROADCAST = $00000002;
  IFF_LOOPBACK = $00000004;
  IFF_POINTTOPOINT = $00000008;
  IFF_MULTICAST = $00000010;

type
  sockaddr_gen = packed record
    AddressIn: sockaddr_in;
    filler: packed array [0..7] of char;
  end;

  INTERFACE_INFO = packed record
    iiFlags: u_long; // ����� ����������
    iiAddress: sockaddr_gen; // ����� ����������
    iiBroadcastAddress: sockaddr_gen; // Broadcast �����
    iiNetmask: sockaddr_gen; // ����� �������
  end;

function WSAIoctl(s: TSocket; cmd: DWORD; lpInBuffer: PCHAR; dwInBufferLen: DWORD;
  lpOutBuffer: PCHAR; dwOutBufferLen: DWORD; lpdwOutBytesReturned: LPDWORD; lpOverLapped: POINTER;
  lpOverLappedRoutine: POINTER): Integer; stdcall; external 'WS2_32.DLL';

function IPName(const IP: string): string;
function EnumInterfaces(const Output: TStrings): Boolean;

implementation

function IPName(const IP: string): string;
var
  Data: TWSAData;
  AddrIn: TSockAddrIn;
  Host: PHostEnt;
begin
  WSAStartup($101, Data);
  AddrIn.sin_addr.s_addr:= inet_addr(PAnsiChar(AnsiString(IP)));
  Host := gethostbyaddr(@AddrIn.sin_addr.S_addr, 4, AF_INET);
  if Assigned(Host) then Result := string(Host^.h_name)
  else Result := '';
end;

function EnumInterfaces(const Output: TStrings): Boolean;
var
  Data: WSADATA;
  S: TSocket;
  P: Pointer;
  I, J: Integer;
  BytesReturned, SetFlags: u_long;
  AddrIn: TSockAddrIn;
  Text: string;
  Buffer: array[0..20] of INTERFACE_INFO;
begin
  WSAStartup($101, Data); // ��������� WinSock
  try
    S := Socket(AF_INET, SOCK_STREAM, 0); // ��������� �����
    Result := S <> INVALID_SOCKET;
    if Result then
    try
      // �������� WSAIoCtl
      P := @BytesReturned;

      if WSAIoCtl(S, SIO_GET_INTERFACE_LIST, nil, 0, @Buffer, 1024, P, nil, nil) <> SOCKET_ERROR then
      begin // ���� OK, �� ���������� ���������� ������������ �����������

        J := BytesReturned div SizeOf(INTERFACE_INFO);

        for I := 0 to J - 1 do // ��� ������� ����������
        begin
          AddrIn := Buffer[I].iiAddress.addressIn; // IP �����
          Text := string(inet_ntoa(AddrIn.sin_addr));
          Output.Add('IP = ' + Text);
          AddrIn := Buffer[I].iiNetMask.addressIn; // ����� �������
          Text := string(inet_ntoa(AddrIn.sin_addr));
          Output.Add('Mask = ' + Text);
          AddrIn := Buffer[I].iiBroadCastAddress.addressIn; // Broadcast �����
          Text := string(inet_ntoa(AddrIn.sin_addr));
          Output.Add('Broadcast = ' + Text);

          SetFlags := Buffer[I].iiFlags;
          if (SetFlags and IFF_UP) = IFF_UP then
            Output.Add('Interface Up')
          else
            Output.Add('Interface Down');

          if (SetFlags and IFF_BROADCAST) = IFF_BROADCAST then // Broadcasts
            Output.Add('Broadcasts supported')
          else
            Output.Add('Broadcasts not supported');

          if (SetFlags and IFF_LOOPBACK) = IFF_LOOPBACK then // ����������� ���
            Output.Add('Loopback interface')
          else
            Output.Add('Network interface'); // ����������
        end;
      end;
    finally
      CloseSocket(S);
    end;
  finally
    WSACleanUp;
  end;
end;

end.
