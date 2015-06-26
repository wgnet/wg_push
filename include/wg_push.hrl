-record(wg_push_item, {
          id :: integer(),
          device_token :: binary(),
          payload :: binary(),
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
