{ *********************************************************************** }
{                                                                         }
{ IpInformation                                                           }
{                                                                         }
{ Copyright (c) 2013 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit IpInformation;

{$B-}

interface

uses
  Windows, SysUtils, SuperObject;

const
  HostNameIdent = 'HostName';
  DnsIdent = 'DNS';
  NodeTypeIdent = 'NodeType';
  DhcpScopeNameIdent = 'DHCPScopeName';
  RoutingEnabledIdent = 'RoutingEnabled';
  ArpProxyEnabledIdent = 'ARPProxyEnabled';
  DnsEnabledIdent = 'DNSEnabled';
  AdapterIdent = 'Adapter';
  AdapterTypeIdent = 'AdapterType';
  AdapterNameIdent = 'AdapterName';
  DescriptionIdent = 'Description';
  IPIdent = 'IP';
  AddressIdent = 'Address';
  SubnetMaskIdent = 'SubnetMask';
  DhcpEnabledIdent = 'DHCPEnabled';
  DhcpServerIdent = 'DHCPServer';
  LeaseObtainedIdent = 'LeaseObtained';
  LeaseExpiresIdent = 'LeaseExpires';
  HaveWinsIdent = 'HaveWINS';
  PrimaryWinsIdent = 'PrimaryWINSServer';
  SecondaryWinsIdent = 'SecondaryWINSServer';
  GatewayIdent = 'Gateway';

function GetIp: ISuperObject;

implementation

uses
  DateUtils, IpHlpApi, IpIfConst, IpTypes, TextConsts;

function GetIp: ISuperObject;
var
  Size: Longword;
  I: Integer;
  FixedInfo: PFIXED_INFO;
  IP: PIP_ADDR_STRING;
  AdapterInfo, SubInfo: PIP_ADAPTER_INFO;
  ASub, BSub: ISuperObject;
  S: string;
begin
  Result := SO;
  Size := 0;
  I := GetNetworkParams(nil, Size);
  if I = ERROR_BUFFER_OVERFLOW then
  begin
    GetMem(FixedInfo, Size);
    try
      GetNetworkParams(FixedInfo, Size);
      Result.S[HostNameIdent] := string(FixedInfo.HostName);
      Result.O[DnsIdent] := SA([]);
      IP := @FixedInfo.DnsServerList;
      I := 0;
      while Assigned(IP) do
      begin
        Result.A[DnsIdent].S[I] := string(IP.IpAddress.S);
        IP := FixedInfo.DnsServerList.Next;
        Inc(I);
      end;
      case FixedInfo.NodeType of
        BROADCAST_NODETYPE: Result.S[NodeTypeIdent] := 'BROADCAST_NODETYPE';
        PEER_TO_PEER_NODETYPE: Result.S[NodeTypeIdent] := 'PEER_TO_PEER_NODETYPE';
        MIXED_NODETYPE: Result.S[NodeTypeIdent] := 'MIXED_NODETYPE';
        HYBRID_NODETYPE: Result.S[NodeTypeIdent] := 'HYBRID_NODETYPE';
      end;
      Result.S[DhcpScopeNameIdent] := string(FixedInfo.ScopeId);
      Result.B[RoutingEnabledIdent] := LongBool(FixedInfo.EnableRouting);
      Result.B[ArpProxyEnabledIdent] := LongBool(FixedInfo.EnableProxy);
      Result.B[DnsEnabledIdent] := LongBool(FixedInfo.EnableDns);
    finally
      FreeMem(FixedInfo);
    end;
    Size := 0;
    I := GetAdaptersInfo(nil, Size);
    if I = ERROR_BUFFER_OVERFLOW then
    begin
      GetMem(AdapterInfo, Size);
      try
        GetAdaptersInfo(AdapterInfo, Size);
        Result.O[AdapterIdent] := SA([]);
        SubInfo := AdapterInfo;
        while Assigned(SubInfo) do
        begin
          ASub := SO;
          ASub.S[AdapterNameIdent] := string(SubInfo.AdapterName);
          case SubInfo.Type_ of
            MIB_IF_TYPE_OTHER: ASub.S[AdapterTypeIdent] := 'MIB_IF_TYPE_OTHER';
            MIB_IF_TYPE_ETHERNET: ASub.S[AdapterTypeIdent] := 'MIB_IF_TYPE_ETHERNET';
            MIB_IF_TYPE_TOKENRING: ASub.S[AdapterTypeIdent] := 'MIB_IF_TYPE_TOKENRING';
            MIB_IF_TYPE_FDDI: ASub.S[AdapterTypeIdent] := 'MIB_IF_TYPE_FDDI';
            MIB_IF_TYPE_PPP: ASub.S[AdapterTypeIdent] := 'MIB_IF_TYPE_PPP';
            MIB_IF_TYPE_LOOPBACK: ASub.S[AdapterTypeIdent] := 'MIB_IF_TYPE_LOOPBACK';
            MIB_IF_TYPE_SLIP: ASub.S[AdapterTypeIdent] := 'MIB_IF_TYPE_SLIP';
          end;
          ASub.S[DescriptionIdent] := string(SubInfo.Description);
          S := '';
          for I := 0 to SubInfo.AddressLength - 1 do
            if I > 0 then
              S := S + Dot + IntToHex(SubInfo.Address[I], 2)
            else
              S := S + IntToHex(SubInfo.Address[I], 2);
          ASub.S[AddressIdent] := S;
          ASub.O[IPIdent] := SA([]);
          IP := @SubInfo.IpAddressList;
          while Assigned(IP) do
          begin
            BSub := SO;
            BSub.S[AddressIdent] := string(IP.IpAddress.S);
            BSub.S[SubnetMaskIdent] := string(IP.IpMask.S);
            ASub.A[IPIdent].Add(BSub);
            IP := IP.Next;
          end;
          ASub.B[DhcpEnabledIdent] := LongBool(SubInfo.DhcpEnabled);
          ASub.S[DhcpServerIdent] := string(SubInfo.DhcpServer.IpAddress.S);
          if LongBool(SubInfo.DhcpEnabled) then
          begin
            ASub.S[LeaseObtainedIdent] := DateTimeToStr(UnixToDateTime(SubInfo.LeaseObtained));
            ASub.S[LeaseExpiresIdent] := DateTimeToStr(UnixToDateTime(SubInfo.LeaseExpires));
          end;
          ASub.B[HaveWinsIdent] := LongBool(SubInfo.HaveWins);
          ASub.S[PrimaryWinsIdent] := string(SubInfo.PrimaryWinsServer.IpAddress.S);
          ASub.S[SecondaryWinsIdent] := string(SubInfo.SecondaryWinsServer.IpAddress.S);
          ASub.S[GatewayIdent] := string(SubInfo.GatewayList.IpAddress.S);
          Result.A[AdapterIdent].Add(ASub);
          SubInfo := SubInfo.Next;
        end;
      finally
        FreeMem(AdapterInfo);
      end;
    end;
  end;
end;

end.
