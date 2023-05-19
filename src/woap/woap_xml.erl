%%%-------------------------------------------------------------------
%%% @author j-som@foxmail.com
%%% @copyright (C) 2023, Zhakun Game
%%% @doc
%%%
%%% @end
%%% Created : 18. 5月 2023 11:04
%%%-------------------------------------------------------------------
-module(woap_xml).
-author("j-som@foxmail.com").
-include_lib("xmerl/include/xmerl.hrl").
-include("woap_xml.hrl").

-type xml_data() :: #woap_req_data{}.
-export_type([xml_data/0]).

%% API
-export([decode/1, encode/1]).

-export([from_user_name/1, to_user_name/1, create_time/1, msg_type/1, values/2, key_values/2]).

-spec decode(binary()) -> xml_data().
decode(XmlBin) ->
    {XmlElement, _} = xmerl_scan:string(unicode:characters_to_list(XmlBin)),
    Children = xmerl_xpath:string("//*", XmlElement),
    parse(Children, #woap_req_data{}).

-spec encode(xml_data()) -> binary().
encode(Data) ->
    Children = list_to_binary([[atom_to_binary(Key), Val] || {Key, Val} <- maps:to_list(Data)]),
    <<"<xml>", Children/binary, "</xml>">>.

-spec xml_value([#xmlText{}|any()]) -> binary() | float().
xml_value([#xmlText{value = Value, type = cdata}|_]) ->
    list_to_binary(Value);
xml_value([#xmlText{value = Value, type = text}|_]) ->
    case lists:member($., Value) of
        true ->
            list_to_float(Value);
        false ->
            list_to_integer(Value)
    end;
xml_value([_|T]) -> xml_value(T);
xml_value([]) -> undefined.

-spec from_user_name(xml_data()) -> binary().
from_user_name(#{'FromUserName' := Val}) -> Val.

-spec to_user_name(xml_data()) -> binary().
to_user_name(#{'ToUserName' := Val}) -> Val.

-spec create_time(xml_data()) -> binary().
create_time(#{'CreateTime' := Val}) -> Val.

-spec msg_type(xml_data()) -> binary().
msg_type(#{'MsgType' := Val}) -> Val.

-spec values(xml_data(), [atom()]) -> [binary()].
values(Data, Keys) ->
    [maps:get(Key, Data) || Key <- Keys].

-spec key_values(xml_data(), [atom()]) -> [binary()].
key_values(Data, Keys) ->
    [{Key, maps:get(Key, Data)} || Key <- Keys].

parse([], Data) -> Data;
parse([#xmlElement{name = 'ToUserName', content = Content}|Children], Data) ->
    NData = Data#woap_req_data{to_user_name = xml_value(Content)},
    parse(Children, NData);
parse([#xmlElement{name = 'FromUserName', content = Content}|Children], Data) ->
    NData = Data#woap_req_data{from_user_name = xml_value(Content)},
    parse(Children, NData);
parse([#xmlElement{name = 'CreateTime', content = Content}|Children], Data) ->
    NData = Data#woap_req_data{time = xml_value(Content)},
    parse(Children, NData);
parse([#xmlElement{name = 'MsgType', content = Content}|Children], Data) ->
    NData = Data#woap_req_data{msg_type = xml_value(Content)},
    parse(Children, NData);
parse(Children, #woap_req_data{msg_type = undefined} = Data) ->
    {value, #xmlElement{name = 'MsgType', content = Content}, NChildren}
        = lists:keytake('MsgType', #xmlElement.name, Children),
    NData = Data#woap_req_data{msg_type = xml_value(Content)},
    parse(NChildren, NData);
parse(Children, #woap_req_data{msg_type = MsgType, data = undefined} = Data) ->
    {SubData, NChildren} = parse_data(MsgType, Children),
    parse(NChildren, Data#woap_req_data{data = SubData});
parse([#xmlElement{name = TagName}|T], #woap_req_data{msg_type = MsgType} = Data) ->
    wechat_logger:warning("parse xml miss ~s's tag ~s", [MsgType, TagName]),
    parse(T, Data).

parse_data(<<"event">>, Children) ->
    {value, #xmlElement{name = 'Event', content = Content}, Children1}
        = lists:keytake('Event', #xmlElement.name, Children),
    Event =
        case xml_value(Content) of
            <<"CLICK">> ->
                #woap_event_click{};
            <<"VIEW">> ->
                #woap_event_view{};
            <<"scancode_push">> ->
                #woap_event_scancode_push{};
            <<"scancode_waitmsg">> ->
                #woap_event_scancode_waitmsg{};
            <<"pic_sysphoto">> ->
                #woap_event_pic_sysphoto{};
            <<"pic_photo_or_album">> ->
                #woap_event_scancode_push{};
            <<"pic_weixin">> ->
                #woap_event_pic_weixin{};
            <<"location_select">> ->
                #woap_location_info{};
            <<"view_miniprogram">> ->
                #woap_event_view_miniprogram{};
            UnknownEvent ->
                wechat_logger:warning("miss event ~s", [UnknownEvent]),
                undefined
        end,
    parse_event(Event, Children1, []);

parse_data(<<"text">>, Children) ->
    parse_msg_text(Children, #woap_msg_text{}, []);
parse_data(<<"image">>, Children) ->
    parse_msg_image(Children, #woap_msg_image{}, []);
parse_data(<<"voice">>, Children) ->
    parse_msg_voice(Children, #woap_msg_voice{}, []);
parse_data(<<"video">>, Children) ->
    parse_msg_video(Children, #woap_msg_video{}, []);
parse_data(<<"shortvideo">>, Children) ->
    parse_msg_video(Children, #woap_msg_video{}, []);
parse_data(<<"location">>, Children) ->
    parse_msg_location(Children, #woap_msg_location{}, []);
parse_data(<<"link">>, Children) ->
    parse_msg_link(Children, #woap_msg_link{}, []);
parse_data(MsgType, Children) ->
    wechat_logger:warning("parse msg_type miss ~s", [MsgType]),
    {none, Children}.



parse_event([], Data, Children) ->
    {Data, Children};
parse_event(#woap_event_click{} = Data, [H|Children], Rest) ->
    case H of
        #xmlElement{name = 'EventKey', content = Content} ->
            parse_event(Data#woap_event_click{key = xml_value(Content)}, Children, Rest);
        _ ->
            parse_event(Data, Children, [H|Rest])
    end;
parse_event(#woap_event_view{} = Data, [H|Children], Rest) ->
    case H of
        #xmlElement{name = 'EventKey', content = Content} ->
            parse_event(Data#woap_event_view{url = xml_value(Content)}, Children, Rest);
        #xmlElement{name = 'MenuId', content = Content} ->
            parse_event(Data#woap_event_view{menu_id = xml_value(Content)}, Children, Rest);
        _ ->
            parse_event(Data, Children, [H|Rest])
    end;
parse_event(#woap_event_scancode_push{} = Data, [H|Children], Rest) ->
    case H of
        #xmlElement{name = 'EventKey', content = Content} ->
            parse_event(Data#woap_event_scancode_push{args = xml_value(Content)}, Children, Rest);
        #xmlElement{name = 'ScanCodeInfo', content = Content} ->
            parse_event(Data#woap_event_scancode_push{info = parse_scancode_info(Content, #woap_scancode_info{})}, Children, Rest);
        _ ->
            parse_event(Data, Children, [H|Rest])
    end;
parse_event(#woap_event_scancode_waitmsg{} = Data, [H|Children], Rest) ->
    case H of
        #xmlElement{name = 'EventKey', content = Content} ->
            parse_event(Data#woap_event_scancode_waitmsg{args = xml_value(Content)}, Children, Rest);
        #xmlElement{name = 'ScanCodeInfo', content = Content} ->
            parse_event(Data#woap_event_scancode_waitmsg{info = parse_scancode_info(Content, #woap_scancode_info{})}, Children, Rest);
        _ ->
            parse_event(Data, Children, [H|Rest])
    end;
parse_event(#woap_event_pic_sysphoto{} = Data, [H|Children], Rest) ->
    case H of
        #xmlElement{name = 'EventKey', content = Content} ->
            parse_event(Data#woap_event_pic_sysphoto{args = xml_value(Content)}, Children, Rest);
        #xmlElement{name = 'SendPicsInfo', content = Content} ->
            parse_event(Data#woap_event_pic_sysphoto{info = parse_pics_info(Content, #woap_pics_info{})}, Children, Rest);
        _ ->
            parse_event(Data, Children, [H|Rest])
    end;
parse_event(#woap_event_pic_photo_or_album{} = Data, [H|Children], Rest) ->
    case H of
        #xmlElement{name = 'EventKey', content = Content} ->
            parse_event(Data#woap_event_pic_photo_or_album{args = xml_value(Content)}, Children, Rest);
        #xmlElement{name = 'SendPicsInfo', content = Content} ->
            parse_event(Data#woap_event_pic_photo_or_album{info = parse_pics_info(Content, #woap_pics_info{})}, Children, Rest);
        _ ->
            parse_event(Data, Children, [H|Rest])
    end;
parse_event(#woap_event_pic_weixin{} = Data, [H|Children], Rest) ->
    case H of
        #xmlElement{name = 'EventKey', content = Content} ->
            parse_event(Data#woap_event_pic_weixin{args = xml_value(Content)}, Children, Rest);
        #xmlElement{name = 'SendPicsInfo', content = Content} ->
            parse_event(Data#woap_event_pic_weixin{info = parse_pics_info(Content, #woap_pics_info{})}, Children, Rest);
        _ ->
            parse_event(Data, Children, [H|Rest])
    end;
parse_event(#woap_event_location_select{} = Data, [H|Children], Rest) ->
    case H of
        #xmlElement{name = 'EventKey', content = Content} ->
            parse_event(Data#woap_event_location_select{args = xml_value(Content)}, Children, Rest);
        #xmlElement{name = 'SendLocationInfo', content = Content} ->
            {Info, _} = parse_location(Content, #woap_location_info{}, []),
            parse_event(Data#woap_event_location_select{info = Info}, Children, Rest);
        _ ->
            parse_event(Data, Children, [H|Rest])
    end;


parse_event(#woap_event_view_miniprogram{} = Data, [H|Children], Rest) ->
    case H of
        #xmlElement{name = 'EventKey', content = Content} ->
            parse_event(Data#woap_event_view_miniprogram{path = xml_value(Content)}, Children, Rest);
        #xmlElement{name = 'MenuId', content = Content} ->
            parse_event(Data#woap_event_view_miniprogram{menu_id = xml_value(Content)}, Children, Rest);
        _ ->
            parse_event(Data, Children, [H|Rest])
    end;

parse_event(Data, [H|Children], Rest) ->
    parse_event(Children, Data, [H|Rest]).

parse_scancode_info([#xmlElement{name = 'ScanType', content = Content}|T], Info) ->
    parse_scancode_info(T, Info#woap_scancode_info{type = xml_value(Content)});
parse_scancode_info([#xmlElement{name = 'ScanResult', content = Content}|T], Info) ->
    parse_scancode_info(T, Info#woap_scancode_info{data = xml_value(Content)});
parse_scancode_info([#xmlElement{name = TagName, content = Content}|T], Info) ->
    wechat_logger:warning("parse_scancode_info ~s, ~p", [TagName, xml_value(Content)]),
    parse_scancode_info(T, Info);
parse_scancode_info([], Info) ->
    Info.

parse_pics_info([#xmlElement{name = 'Count', content = Content}|T], Info) ->
    parse_pics_info(T, Info#woap_pics_info{count = xml_value(Content)});
parse_pics_info([#xmlElement{name = 'PicList', content = Content}|T], Info) ->
    parse_pics_info(T, Info#woap_pics_info{list = parse_pics_list(Content, [])});
parse_pics_info([#xmlElement{name = TagName, content = Content}|T], Info) ->
    wechat_logger:warning("parse_pics_info ~s, ~p", [TagName, xml_value(Content)]),
    parse_pics_info(T, Info);
parse_pics_info([], Info) ->
    Info.

parse_pics_list([#xmlElement{name = 'item', content = Content}|T], Items) ->
    parse_pics_list(T, [parse_pic_item(Content, #woap_pic_item{})|Items]);
parse_pics_list([#xmlElement{name = TagName, content = Content}|T], Items) ->
    wechat_logger:warning("parse_pics_list ~s, ~p", [TagName, xml_value(Content)]),
    parse_pics_list(T, Items);
parse_pics_list([], Items) ->
    Items.

parse_pic_item([#xmlElement{name = 'PicMd5Sum', content = Content}|T], Item) ->
    parse_pic_item(T, Item#woap_pic_item{md5 = xml_value(Content)});
parse_pic_item([#xmlElement{name = TagName, content = Content}|T], Item) ->
    wechat_logger:warning("parse_pic_item ~s, ~p", [TagName, xml_value(Content)]),
    parse_pic_item(T, Item);
parse_pic_item([], Item) ->
    Item.

parse_location([#xmlElement{name = 'Location_X', content = Content}|T], Loc, Rest) ->
    parse_location(T, Loc#woap_location_info{x = xml_value(Content)}, Rest);
parse_location([#xmlElement{name = 'Location_Y', content = Content}|T], Loc, Rest) ->
    parse_location(T, Loc#woap_location_info{y = xml_value(Content)}, Rest);
parse_location([#xmlElement{name = 'Scale', content = Content}|T], Loc, Rest) ->
    parse_location(T, Loc#woap_location_info{scale = xml_value(Content)}, Rest);
parse_location([#xmlElement{name = 'Label', content = Content}|T], Loc, Rest) ->
    parse_location(T, Loc#woap_location_info{label = xml_value(Content)}, Rest);
parse_location([#xmlElement{name = 'Poiname', content = Content}|T], Loc, Rest) ->
    parse_location(T, Loc#woap_location_info{poi_name = xml_value(Content)}, Rest);
parse_location([H|T], Loc, Rest) ->
%%    wechat_logger:warning("parse_location ~s, ~p", [TagName, xml_value(Content)], Rest),
    parse_location(T, Loc, [H|Rest]);
parse_location([], Loc, Rest) ->
    {Loc, Rest}.

parse_msg_text([#xmlElement{name = 'Content', content = Content}|T], Data, Rest) ->
    parse_msg_text(T, Data#woap_msg_text{content = xml_value(Content)}, Rest);
parse_msg_text([#xmlElement{name = 'MsgId', content = Content}|T], Data, Rest) ->
    parse_msg_text(T, Data#woap_msg_text{msg_id = xml_value(Content)}, Rest);
parse_msg_text([#xmlElement{name = 'MsgDataId', content = Content}|T], Data, Rest) ->
    parse_msg_text(T, Data#woap_msg_text{msg_data_id = xml_value(Content)}, Rest);
parse_msg_text([#xmlElement{name = 'Idx', content = Content}|T], Data, Rest) ->
    parse_msg_text(T, Data#woap_msg_text{idx = xml_value(Content)}, Rest);
parse_msg_text([H|T], Data, Rest) ->
    parse_msg_text(T, Data, [H|Rest]);
parse_msg_text([], Data, Rest) -> {Data, Rest}.

parse_msg_image([#xmlElement{name = 'PicUrl', content = Content}|T], Data, Rest) ->
    parse_msg_image(T, Data#woap_msg_image{url = xml_value(Content)}, Rest);
parse_msg_image([#xmlElement{name = 'MediaId', content = Content}|T], Data, Rest) ->
    parse_msg_image(T, Data#woap_msg_image{media_id = xml_value(Content)}, Rest);
parse_msg_image([#xmlElement{name = 'MsgId', content = Content}|T], Data, Rest) ->
    parse_msg_image(T, Data#woap_msg_image{msg_id = xml_value(Content)}, Rest);
parse_msg_image([#xmlElement{name = 'MsgDataId', content = Content}|T], Data, Rest) ->
    parse_msg_image(T, Data#woap_msg_image{msg_data_id = xml_value(Content)}, Rest);
parse_msg_image([#xmlElement{name = 'Idx', content = Content}|T], Data, Rest) ->
    parse_msg_image(T, Data#woap_msg_image{idx = xml_value(Content)}, Rest);
parse_msg_image([H|T], Data, Rest) ->
    parse_msg_image(T, Data, [H|Rest]);
parse_msg_image([], Data, Rest) -> {Data, Rest}.

parse_msg_voice([#xmlElement{name = 'Format', content = Content}|T], Data, Rest) ->
    parse_msg_voice(T, Data#woap_msg_voice{format = xml_value(Content)}, Rest);
parse_msg_voice([#xmlElement{name = 'MediaId', content = Content}|T], Data, Rest) ->
    parse_msg_voice(T, Data#woap_msg_voice{media_id = xml_value(Content)}, Rest);
parse_msg_voice([#xmlElement{name = 'MsgId', content = Content}|T], Data, Rest) ->
    parse_msg_voice(T, Data#woap_msg_voice{msg_id = xml_value(Content)}, Rest);
parse_msg_voice([#xmlElement{name = 'MsgDataId', content = Content}|T], Data, Rest) ->
    parse_msg_voice(T, Data#woap_msg_voice{msg_data_id = xml_value(Content)}, Rest);
parse_msg_voice([#xmlElement{name = 'Idx', content = Content}|T], Data, Rest) ->
    parse_msg_voice(T, Data#woap_msg_voice{idx = xml_value(Content)}, Rest);
parse_msg_voice([#xmlElement{name = 'Recognition', content = Content}|T], Data, Rest) ->
    parse_msg_voice(T, Data#woap_msg_voice{recognition = xml_value(Content)}, Rest);
parse_msg_voice([H|T], Data, Rest) ->
    parse_msg_voice(T, Data, [H|Rest]);
parse_msg_voice([], Data, Rest) -> {Data, Rest}.


parse_msg_video([#xmlElement{name = 'ThumbMediaId', content = Content}|T], Data, Rest) ->
    parse_msg_video(T, Data#woap_msg_video{thumb_media_id = xml_value(Content)}, Rest);
parse_msg_video([#xmlElement{name = 'MediaId', content = Content}|T], Data, Rest) ->
    parse_msg_video(T, Data#woap_msg_video{media_id = xml_value(Content)}, Rest);
parse_msg_video([#xmlElement{name = 'MsgId', content = Content}|T], Data, Rest) ->
    parse_msg_video(T, Data#woap_msg_video{msg_id = xml_value(Content)}, Rest);
parse_msg_video([#xmlElement{name = 'MsgDataId', content = Content}|T], Data, Rest) ->
    parse_msg_video(T, Data#woap_msg_video{msg_data_id = xml_value(Content)}, Rest);
parse_msg_video([#xmlElement{name = 'Idx', content = Content}|T], Data, Rest) ->
    parse_msg_video(T, Data#woap_msg_video{idx = xml_value(Content)}, Rest);
parse_msg_video([H|T], Data, Rest) ->
    parse_msg_video(T, Data, [H|Rest]);
parse_msg_video([], Data, Rest) -> {Data, Rest}.

parse_msg_location([#xmlElement{name = 'MsgId', content = Content}|T], Data, Rest) ->
    parse_msg_location(T, Data#woap_msg_location{msg_id = xml_value(Content)}, Rest);
parse_msg_location([#xmlElement{name = 'MsgDataId', content = Content}|T], Data, Rest) ->
    parse_msg_location(T, Data#woap_msg_location{msg_data_id = xml_value(Content)}, Rest);
parse_msg_location([#xmlElement{name = 'Idx', content = Content}|T], Data, Rest) ->
    parse_msg_location(T, Data#woap_msg_location{idx = xml_value(Content)}, Rest);
parse_msg_location(Children, #woap_msg_location{info = undefined} = Data, Rest) ->
    {Info, NChildren} = parse_location(Children, #woap_location_info{}, []),
    parse_msg_location(NChildren, Data#woap_msg_location{info = Info}, Rest);
parse_msg_location([H|T], Data, Rest) ->
    parse_msg_location(T, Data, [H|Rest]);
parse_msg_location([], Data, Rest) -> {Data, Rest}.

parse_msg_link([#xmlElement{name = 'Title', content = Content}|T], Data, Rest) ->
    parse_msg_link(T, Data#woap_msg_link{title = xml_value(Content)}, Rest);
parse_msg_link([#xmlElement{name = 'Description', content = Content}|T], Data, Rest) ->
    parse_msg_link(T, Data#woap_msg_link{desc = xml_value(Content)}, Rest);
parse_msg_link([#xmlElement{name = 'Url', content = Content}|T], Data, Rest) ->
    parse_msg_link(T, Data#woap_msg_link{url = xml_value(Content)}, Rest);
parse_msg_link([#xmlElement{name = 'MsgId', content = Content}|T], Data, Rest) ->
    parse_msg_link(T, Data#woap_msg_link{msg_id = xml_value(Content)}, Rest);
parse_msg_link([#xmlElement{name = 'MsgDataId', content = Content}|T], Data, Rest) ->
    parse_msg_link(T, Data#woap_msg_link{msg_data_id = xml_value(Content)}, Rest);
parse_msg_link([#xmlElement{name = 'Idx', content = Content}|T], Data, Rest) ->
    parse_msg_link(T, Data#woap_msg_link{idx = xml_value(Content)}, Rest);
parse_msg_link([H|T], Data, Rest) ->
    parse_msg_link(T, Data, [H|Rest]);
parse_msg_link([], Data, Rest) -> {Data, Rest}.