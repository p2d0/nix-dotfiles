{ config, lib, pkgs, ... }:
# Older versions
# https://lazamar.co.uk/nix-versions/
with lib;
let cfg = config.modules.vpn;
    oldPkgs = import (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/c2c0373ae7abf25b7d69b2df05d3ef8014459ea3.tar.gz";
    }) {};
in {
  options.modules.vpn = {
    enable = mkOption {
      type = types.bool;
      default = false;
      example = "";
      description = ''
      '';
    };
  };

  config = mkIf cfg.enable (lib.my.withHome
    (args: {
      home.file = {
        ".config/qv2ray".source = args.config.lib.file.mkOutOfStoreSymlink /home/${config.user}/Dropbox/qv2ray;
      };})
    {
      environment.systemPackages = with pkgs;
        [
          pkgs.v2ray
          pkgs.qv2ray
          pkgs.unstable.nekoray
          pkgs.xray
          # my.psiphon
          # my.lantern
        ];

      systemd.services.xray = {
        enable = true;
        description = "Xray";
        wantedBy = [ "default.target" ];
        serviceConfig = {
          Type = "simple";
          ExecStart = let
            config = (pkgs.writeText "xray_config.json" (builtins.toJSON
              { dns =
                  { disableFallback = true; servers = [
                      { address = "https://8.8.8.8/dns-query"; domains = [ ]; queryStrategy = ""; }
                      { address = "localhost"; domains = [ "full:nakhmetov.mooo.com" ]; queryStrategy = ""; } ]; tag = "dns"; };
                inbounds = [
                  { listen = "127.0.0.1"; port = 8091; protocol = "socks"; settings =
                      { udp = true; }; sniffing =
                          { destOverride = [ "http" "tls" "quic" ]; enabled = true; metadataOnly = false; routeOnly = true; }; tag = "socks-in"; }
                  { listen = "127.0.0.1"; port = 8092; protocol = "http"; sniffing =
                      { destOverride = [ "http" "tls" "quic" ]; enabled = true; metadataOnly = false; routeOnly = true; }; tag = "http-in"; } ];
                log =
                  { loglevel = "warning"; };
                outbounds = [
                  { QV2RAY_OUTBOUND_METADATA = { displayName = "test"; };
                    _QV2RAY_USE_GLOBAL_FORWARD_PROXY_ = false;
                    mux = { concurrency = 1; enabled = false; };
                    protocol = "shadowsocks"; sendThrough = "0.0.0.0";
                    #TODO save pass in cachenix?
                    settings = { servers = [ { address = "pogpogvpnvpnpogpog.fly.dev"; method = "chacha20-ietf-poly1305"; password = "barbus007"; port = 443; } ]; };
                    streamSettings = { network = "ws"; security = "tls"; wsSettings = { path = "/v2"; }; }; tag = "Personal"; }
                  { QV2RAY_OUTBOUND_METADATA = { displayName = "BlackHole-59542493"; };
                    _QV2RAY_USE_GLOBAL_FORWARD_PROXY_ = false;
                    mux = { concurrency = 1; enabled = null; };
                    protocol = "http";
                    sendThrough = "0.0.0.0";
                    settings = { servers = [ { address = "localhost"; port = 9054; } ]; }; streamSettings = { }; tag = "WARP"; }
                  { domainStrategy = "AsIs"; flow = null; protocol = "vless";
                    settings =
                      { vnext = [
                          { address = "nakhmetov.mooo.com"; port = 443; users = [
                              { encryption = "none"; flow = ""; id = "a040e4f3-bf1a-4958-adab-7de36d3ac99a"; } ]; } ]; };
                    streamSettings =
                      { network = "tcp"; realitySettings =
                          { fingerprint = "firefox"; publicKey = "n8xsPPYPcxk8VLkFmH1dQFJn-FIusBQStVY3kSdE3lg"; serverName = "telegram.org"; shortId = "00e715da"; spiderX = "/"; }; security = "reality"; };
                    tag = "ahmet-vless"; }
                  { domainStrategy = ""; protocol = "freedom"; tag = "direct"; }
                  { domainStrategy = ""; protocol = "freedom"; tag = "bypass"; }
                  { protocol = "blackhole"; tag = "block"; }
                  { protocol = "dns"; proxySettings =
                      { tag = "proxy"; transportLayer = true; }; settings =
                          { address = "8.8.8.8"; network = "tcp"; port = 53; userLevel = 1; }; tag = "dns-out"; } ];
                policy =
                  { levels =
                      { "1" =
                          { connIdle = 30; }; }; system =
                              { statsOutboundDownlink = true; statsOutboundUplink = true; }; };
                routing =
                  { domainStrategy = "AsIs";
                    rules = [
                      { QV2RAY_RULE_ENABLED = true; QV2RAY_RULE_TAG = "WARP";
                        domain = [ "1377x.to"
                                   "2ip.ru"
                                   "4chan.org"
                                   "7tv.app"
                                   "all3dp.com"
                                   "archiveofourown.org"
                                   "bbc.com"
                                   "beekeeb.com"
                                   "bleachbubble.com"
                                   "botostore.com"
                                   "capcut.com"
                                   "catbox.moe"
                                   "cbf2a2c9.landing-jvi.pages.dev"
                                   "cdninstagram"
                                   "cgscomputer.com"
                                   "chess.com"
                                   "chesscomfiles.com\t"
                                   "clevelandclinic.org"
                                   "crunchbase.com"
                                   "currenttime.tv"
                                   "deepstatemap.live"
                                   "dell.com"
                                   "dw.com"
                                   "echofm.online"
                                   "findbook.in.ua"
                                   "forefront.ai"
                                   "gambody.com"
                                   "gelbooru.com"
                                   "getsby.com"
                                   "gfycat.com"
                                   "hemkop.se"
                                   "hey.com"
                                   "hubspot.com"
                                   "i.kym-cdn.com"
                                   "kickasstorrents.to"
                                   "knowyourmeme.com"
                                   "kyivpost.com"
                                   "link.stremio.com"
                                   "linked.in"
                                   "linkedin.com"
                                   "linktr.ee"
                                   "liveuamap.com"
                                   "mangadex.org"
                                   "medium.com"
                                   "meduza.io"
                                   "metacritic.com"
                                   "national.com"
                                   "nnmclub.to"
                                   "novayagazeta.eu"
                                   "oculus.com"
                                   "opensea.io"
                                   "organizing.tv"
                                   "ovdinfo.org"
                                   "paperpaper.ru"
                                   "pixiv.net"
                                   "play.google.com"
                                   "poe.com"
                                   "polygraph.info"
                                   "pravda.com.ua"
                                   "premierleague.com"
                                   "proton.me"
                                   "qelectrotech.org"
                                   "qualcomm.com"
                                   "quora.com"
                                   "robotcache.com"
                                   "rutracker.org"
                                   "seagate.com"
                                   "sibreal.org"
                                   "singular.net"
                                   "soundcloud.com"
                                   "stripe.com"
                                   "svoboda.org"
                                   "t-invariant.org"
                                   "telegramcatalog.com"
                                   "telegraph.co.uk"
                                   "theins.ru"
                                   "themoscowtimes.com"
                                   "thepiratebay.org"
                                   "thepiratebay.party"
                                   "theseasonedmom.com"
                                   "thingiverse.com"
                                   "threadreaderapp.com"
                                   "threads.net"
                                   "twimg.co"
                                   "twitter.com"
                                   "typing.com"
                                   "unl.edu"
                                   "upsellit.com"
                                   "verstka.media"
                                   "whatismyipaddress.com"
                                   "yeggi.com"
                                   "zedge.net"
                                 ];
                        inboundTag = [ "socks-in" ];
                        outboundTag = "WARP";
                        type = "field"; }
                      { QV2RAY_RULE_ENABLED = true; QV2RAY_RULE_TAG = "US";
                        domain = [ "ai.google.dev"
                                   "aistudio.google.com"
                                   "alkalilogexporter-pa.clients6.google.com"
                                   "anthropic.com"
                                   "bard.google.com"
                                   "bitbucket.org"
                                   "bosch-home.com."
                                   "boto.io"
                                   "chatgpt.com"
                                   "claude.ai"
                                   "clients6.google.com"
                                   "content-developerprofiles-pa.googleapis.com"
                                   "cpu-monkey.com"
                                   "developers.google.com"
                                   "docs.google.com"
                                   "emojipedia.org"
                                   "facebook.com"
                                   "fbcdn.net"
                                   "geosite:facebook"
                                   "healthline.com"
                                   "hub.docker.com"
                                   "humanscale.com"
                                   "importgenius.com"
                                   "importkey.com"
                                   "instagram.com"
                                   "intel.com"
                                   "iplocation.net"
                                   "meta.ai"
                                   "meta.com\t"
                                   "mistral.ai"
                                   "myfonts.com"
                                   "pimeyes.com"
                                   "routenote.com"
                                   "smarty.net"
                                   "steamidfinder.com"
                                   "store.epicgames.com"
                                   "tsn.ua"
                                   "upgrade.com"
                                   "waa-pa.clients6.google.com"
                                 ];
                        inboundTag = [ "socks-in"];
                        outboundTag = "ahmet-vless";
                        type = "field";}
                      { QV2RAY_RULE_ENABLED = true; QV2RAY_RULE_TAG = "http proxy"; inboundTag = [ "http-in" ]; network = "udp,tcp"; outboundTag = "ahmet-vless"; type = "field"; }
                      { QV2RAY_RULE_ENABLED = true; QV2RAY_RULE_TAG = "OtherVPN";
                        domain = [ "bing.com"
                                   "bingapis.com"
                                   "files.oaiusercontent.com"
                                   "live.com"
                                   "microsoftonline.com"
                                   "msftauth.net"
                                   "oaistatic.com"
                                   "openai.com"
                                   "rustdesk.com"
                                   "spotify.com"
                                   "thenextweb.com" ];
                        inboundTag = [ "socks-in" ];
                        network = "udp,tcp";
                        outboundTag = "ahmet-vless";
                        type = "field"; }
                      { QV2RAY_RULE_ENABLED = true; QV2RAY_RULE_TAG = "Personal";
                        domain = [ "microsoft.com" "tiktok.com" "whatismyip.com" ];
                        inboundTag = [ "socks-in" ];
                        network = "udp,tcp";
                        outboundTag = "Personal"; type = "field"; }
                      { QV2RAY_RULE_ENABLED = true; QV2RAY_RULE_TAG = "Default";
                        inboundTag = [ "socks-in" ]; outboundTag = "direct"; type = "field"; }
                      { inboundTag = [ "socks-in" "http-in" ]; outboundTag = "dns-out"; port = "53"; type = "field"; }
                      { outboundTag = "proxy"; port = "0-65535"; type = "field"; } ]; };
                stats = { };
              }) );
          in "${pkgs.xray}/bin/xray run -c ${config}";
        };
      };});
}
