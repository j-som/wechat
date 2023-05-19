%%%-------------------------------------------------------------------
%%% @author j-som@foxmail.com
%%% @copyright (C) 2023, Zhakun Game
%%% @doc
%%% 微信服务器推过来的数据格式
%%% @end
%%% Created : 18. 5月 2023 14:44
%%%-------------------------------------------------------------------
-author("j-som@foxmail.com").

%% 位置信息
-record(woap_location_info, {
    x,          % X坐标信息
    y,          % Y坐标信息
    scale,      % 精度，可理解为精度或者比例尺、越精细的话 scale越高
    label,      % 地理位置的字符串信息 <[CDATA[ 广州市海珠区客村艺苑路 106号]]>
    poi_name    % 朋友圈POI的名字，可能为空
}).

%% 发送的图片列表信息
-record(woap_pics_info, {
    count,      % 发送的图片数量
    list        % 图片列表
}).

%% 单个图片信息
-record(woap_pic_item, {
    md5         % 图片的MD5值，开发者若需要，可用于验证接收到图片
}).

% 扫描信息
-record(woap_scancode_info, {
    type,       % 扫描类型，一般是qrcode
    data        % 扫描结果，即二维码对应的字符串信息
}).

-record(woap_req_data, {
    to_user_name,       % 开发者微信号 gh_e136c6e50636
    from_user_name,     % 接收方帐号（收到的OpenID）
    time,               % 消息创建时间 （整型）
    msg_type,           % 消息类型
    data                % 根据消息类型不同有不同的结构，入下
}).

%% 点击菜单拉取消息时的事件推送
-record(woap_event_click, {
    key         % 事件KEY值，与自定义菜单接口中KEY值对应
}).

%% 点击菜单跳转链接时的事件推送
-record(woap_event_view, {
    url,        % 设置的跳转URL
    menu_id     % 指菜单ID，如果是个性化菜单，则可以通过这个字段，知道是哪个规则的菜单被点击了。
}).

%% 扫码推事件的事件推送
-record(woap_event_scancode_push, {
    info = #woap_scancode_info{},       % 扫描信息
    args        % 由开发者在创建菜单时设定
}).

-record(woap_event_scancode_waitmsg, {
    info = #woap_scancode_info{},       % 扫描信息
    args        % 由开发者在创建菜单时设定
}).

%% 弹出系统拍照发图的事件推送
-record(woap_event_pic_sysphoto, {
    info,       % #woap_pics_info{}
    args
}).

%% 弹出拍照或者相册发图的事件推送
-record(woap_event_pic_photo_or_album, {
    info,       % #woap_pics_info{}
    args
}).

%% 弹出微信相册发图器的事件推送
-record(woap_event_pic_weixin, {
    info,       % #woap_pics_info{}
    args
}).

%% 点击菜单跳转小程序的事件推送
-record(woap_event_view_miniprogram, {
    path,       % 跳转的小程序路径
    menu_id     % 菜单ID，如果是个性化菜单，则可以通过这个字段，知道是哪个规则的菜单被点击了
}).

%% 关注/取消关注事件
-record(woap_event_subscribe, {
    op :: 0|1,      % 0为取消 1为关注
    % 如果用户还未关注公众号，扫描带参数二维码事件, 则用户可以关注公众号，关注后微信会将带场景值关注事件推送给开发者。
    info            % #woap_event_qr_scene{}
}).

%% 带参数二维码 用户已关注时的事件推送 SCAN
-record(woap_event_qr_scene, {
    params,         % 二维码的参数值
    tiket           % 二维码的ticket，可用来换取二维码图片
}).

%% 上报地理位置事件 LOCATION
%% 用户同意上报地理位置后，每次进入公众号会话时，都会在进入时上报地理位置，或在进入会话后每5秒上报一次地理位置，公众号可以在公众平台网站中修改以上设置。
%% 上报地理位置时，微信会将上报地理位置事件推送到开发者填写的URL。
-record(woap_event_location, {
    latitude,       % 地理位置纬度
    longitude,      % 地理位置经度
    precision       % 地理位置精度
}).


%% 发送的位置信息
-record(woap_event_location_select, {
    info,       % #woap_location_info{}
    args
}).

%% 文本消息
-record(woap_msg_text, {
    content,        % 文本消息内容
    msg_id,         % 消息id，64位整型
    msg_data_id,    % 消息的数据ID（消息如果来自文章时才有）
    idx             % 多图文时第几篇文章，从1开始（消息如果来自文章时才有）
}).

%% 图片消息
-record(woap_msg_image, {
    url,            % 图片链接（由系统生成）
    media_id,       % 图片消息媒体id，可以调用获取临时素材接口拉取数据。
    msg_id,         % 消息id，64位整型
    msg_data_id,    % 消息的数据ID（消息如果来自文章时才有）
    idx             % 多图文时第几篇文章，从1开始（消息如果来自文章时才有）
}).

%% 语音消息
-record(woap_msg_voice, {
    media_id,       % 语音消息媒体id，可以调用获取临时素材接口拉取数据。
    format,         % 语音格式，如amr，speex等
    msg_id,         % 消息id，64位整型
    msg_data_id,    % 消息的数据ID（消息如果来自文章时才有）
    idx,            % 多图文时第几篇文章，从1开始（消息如果来自文章时才有）
    recognition     % 语音识别结果，UTF8编码 可选的
}).

%% 视频消息
-record(woap_msg_video, {
    media_id,       % 视频消息媒体id，可以调用获取临时素材接口拉取数据。
    thumb_media_id, % 视频消息缩略图的媒体id，可以调用多媒体文件下载接口拉取数据。
    msg_id,         % 消息id，64位整型
    msg_data_id,    % 消息的数据ID（消息如果来自文章时才有）
    idx             % 多图文时第几篇文章，从1开始（消息如果来自文章时才有）

}).

%% 地理位置消息
-record(woap_msg_location, {
    info,           %  #woap_location_info{}
    msg_id,         % 消息id，64位整型
    msg_data_id,    % 消息的数据ID（消息如果来自文章时才有）
    idx             % 多图文时第几篇文章，从1开始（消息如果来自文章时才有）
}).

%% 链接消息
-record(woap_msg_link, {
    title,          % 消息标题
    desc,           % 消息描述
    url,            % 消息链接
    msg_id,         % 消息id，64位整型
    msg_data_id,    % 消息的数据ID（消息如果来自文章时才有）
    idx             % 多图文时第几篇文章，从1开始（消息如果来自文章时才有）
}).
