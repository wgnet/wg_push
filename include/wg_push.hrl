-record(wg_push_alert, {
    title :: binary(),
    body :: binary(),
    title_loc_key :: binary(),
    title_loc_args :: [binary()],
    action_loc_key :: binary(),
    loc_key :: binary(),
    loc_args :: [binary()],
    launch_image :: binary()
}).

-record(wg_push_aps, {
    alert :: #wg_push_alert{} | binary(),
    badge :: integer(),
    sound :: binary(),
    content_available :: integer(),
    data :: proplist:proplist()
}).

-record(wg_push_item, {
    id :: integer(),
    device_token :: binary(),
    payload :: #wg_push_aps{} | binary(),
    expiration_date :: integer(),
    priority = 5 :: integer()
}).

-record(wg_push_ssl_options, {
    certfile :: file:name_all(),
    keyfile :: file:name_all(),
    cert :: binary(),
    key :: atom() | binary(),
    password :: string(),
    versions :: [atom()] % [sslv3 | tlsv1 | 'tlsv1.1' | 'tlsv1.2']
}).

