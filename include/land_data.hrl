-ifndef(__LAND_DATA_PIQI_HRL__).
-define(__LAND_DATA_PIQI_HRL__, 1).

-record(land_data_land_message, {
    id :: string() | binary(),
    friend_data :: land_data_land_message_friend_data(),
    user_data :: land_data_land_message_user_data(),
    inner_land_data :: land_data_land_message_inner_land_data(),
    roads_data :: land_data_land_message_terrain_data(),
    rivers_data :: land_data_land_message_terrain_data(),
    building_data = [] :: [land_data_land_message_building_data()],
    character_data = [] :: [land_data_land_message_character_data()],
    consumable_data = [] :: [land_data_land_message_consumable_data()],
    job_data = [] :: [land_data_land_message_job_data()],
    quest_data = [] :: [land_data_land_message_quest_data()],
    notification_data = [] :: [land_data_land_message_notification_data()],
    inventory_item_data = [] :: [land_data_land_message_inventory_item_data()]
}).
-record(land_data_land_message_friend_data, {
    data_version :: integer(),
    has_lemon_tree :: boolean(),
    language :: integer(),
    level :: integer(),
    name :: string() | binary(),
    rating :: integer()
}).
-record(land_data_land_message_entity_header, {
    id :: non_neg_integer(),
    type :: land_data_land_message_entity_header_type()
}).
-type(land_data_land_message_entity_header_type() :: 
      user
    | innerland
    | roads
    | rivers
    | building
    | character
    | consumable
    | job
    | quest
    | notification
    | inventory
).
-record(land_data_land_message_user_data, {
    header :: land_data_land_message_entity_header(),
    last_bonus_collection :: non_neg_integer(),
    level :: non_neg_integer(),
    experience :: non_neg_integer(),
    money :: non_neg_integer(),
    premium_currency :: non_neg_integer(),
    entity_id :: non_neg_integer(),
    saved_rating = [] :: [land_data_land_message_user_data_saved_rating()],
    last_bonus :: binary(),
    user_name :: string() | binary(),
    friends_unlocked :: boolean(),
    memorabilia_unlocked :: boolean(),
    reorganize_unlocked :: boolean(),
    first_purchase :: boolean(),
    sidebar_default_open :: boolean(),
    show_level_up :: boolean()
}).
-record(land_data_land_message_user_data_saved_rating, {
    saved_rating_elem_id :: non_neg_integer(),
    saved_rating_elem :: integer()
}).
-record(land_data_land_message_inner_land_data, {
    header :: land_data_land_message_entity_header(),
    land_blocks :: string() | binary(),
    next_instance_id :: non_neg_integer(),
    num_chars :: non_neg_integer(),
    num_buildings :: non_neg_integer(),
    num_consumables :: non_neg_integer(),
    num_jobs :: non_neg_integer(),
    num_quests :: non_neg_integer(),
    num_notices :: non_neg_integer(),
    num_inventory_items :: non_neg_integer(),
    num_memorabilia_items :: non_neg_integer(),
    num_event_count_lists :: non_neg_integer(),
    num_premium_unlocks :: non_neg_integer(),
    num_action_limits :: non_neg_integer(),
    num_runs_since_promotion :: non_neg_integer(),
    level_of_rate_ask :: non_neg_integer(),
    update_time :: integer(),
    time_spent_playing :: integer(),
    last_level_up_spent_time :: integer(),
    last_level_up_time_stamp :: integer(),
    initial_save_done :: boolean(),
    last_vandalism_feed_posted :: integer(),
    last_steal_building_feed_posted :: integer(),
    last_sideshow_bob_spawned :: integer(),
    next_currency_id :: non_neg_integer()
}).
-record(land_data_land_message_terrain_data, {
    header :: land_data_land_message_entity_header(),
    map_data_size :: non_neg_integer(),
    map_data :: string() | binary()
}).
-record(land_data_land_message_building_data, {
    header :: land_data_land_message_entity_header(),
    building :: non_neg_integer(),
    creation_time :: integer(),
    update_time :: integer(),
    position_x :: number(),
    position_y :: number(),
    flipped :: boolean(),
    build_state :: non_neg_integer(),
    hurry :: boolean(),
    crop_state :: non_neg_integer(),
    bet_active :: boolean(),
    bet_start_time :: integer(),
    bet_choice :: non_neg_integer(),
    winning_bet :: boolean(),
    vandalized :: boolean(),
    namelen :: integer(),
    vandal_name :: string() | binary(),
    is_owner_list :: boolean(),
    owner_list :: land_data_land_message_building_data_owner_list()
}).
-record(land_data_land_message_building_data_owner_list, {
    id :: string() | binary(),
    user_name = [] :: [string() | binary()]
}).
-record(land_data_land_message_character_data, {
    header :: land_data_land_message_entity_header(),
    character :: non_neg_integer(),
    update_time :: integer(),
    position_x :: number(),
    position_y :: number()
}).
-record(land_data_land_message_consumable_data, {
    header :: land_data_land_message_entity_header(),
    consumable :: non_neg_integer()
}).
-record(land_data_land_message_job_data, {
    header :: land_data_land_message_entity_header(),
    job :: non_neg_integer(),
    char_ref :: non_neg_integer(),
    building_ref :: non_neg_integer(),
    update_time :: integer(),
    state :: integer(),
    hurry :: boolean(),
    has_building :: boolean(),
    has_char :: boolean()
}).
-record(land_data_land_message_quest_data, {
    header :: land_data_land_message_entity_header(),
    quest_id :: non_neg_integer(),
    quest_state :: integer(),
    quest_script_state :: integer(),
    num_objectives :: integer(),
    objective_data = [] :: [land_data_land_message_quest_data_objective_data()]
}).
-record(land_data_land_message_quest_data_objective_data, {
    objective_id :: integer(),
    objective_state :: integer(),
    objective_type :: integer(),
    version :: integer(),
    quest_last_ret_val :: boolean(),
    quest_local :: boolean(),
    rushed :: boolean(),
    keep_done :: boolean(),
    block_done_script :: boolean(),
    building_custom_data :: land_data_land_message_quest_data_objective_data_building_custom_data(),
    resources_custom_data :: land_data_land_message_quest_data_objective_data_resources_custom_data(),
    item_count_custom_data :: land_data_land_message_quest_data_objective_data_item_count_custom_data(),
    visit_friend_custom_data :: land_data_land_message_quest_data_objective_data_visit_friend_custom_data()
}).
-record(land_data_land_message_quest_data_objective_data_building_custom_data, {
    qty :: integer()
}).
-record(land_data_land_message_quest_data_objective_data_resources_custom_data, {
    money :: non_neg_integer(),
    premium :: non_neg_integer()
}).
-record(land_data_land_message_quest_data_objective_data_item_count_custom_data, {
    count :: integer()
}).
-record(land_data_land_message_quest_data_objective_data_visit_friend_custom_data, {
    num :: integer(),
    fake_included :: boolean(),
    friend = [] :: [string() | binary()]
}).
-record(land_data_land_message_notification_data, {
    header :: land_data_land_message_entity_header(),
    building_id :: non_neg_integer(),
    character_id :: non_neg_integer(),
    type :: non_neg_integer(),
    callback_id :: non_neg_integer(),
    start_x :: number(),
    start_y :: number(),
    start_z :: number(),
    end_x :: number(),
    end_y :: number(),
    end_z :: number(),
    move_time :: integer(),
    is_static :: boolean(),
    is_setup :: boolean(),
    has_building :: boolean(),
    has_char :: boolean(),
    history_type :: non_neg_integer(),
    history_job_id :: non_neg_integer(),
    history_building_id :: non_neg_integer(),
    history_character_id :: non_neg_integer(),
    history_quest_id :: non_neg_integer(),
    history_building_instance_id :: non_neg_integer(),
    consumable_callback :: land_data_land_message_notification_data_consumable_callback(),
    memorabilia_reward_callback :: land_data_land_message_notification_data_memorabilia_reward_callback(),
    notification_callback :: land_data_land_message_notification_data_notification_callback()
}).
-record(land_data_land_message_notification_data_consumable_callback, {
    consumable_id :: non_neg_integer(),
    source_length :: integer(),
    source :: string() | binary()
}).
-record(land_data_land_message_notification_data_memorabilia_reward_callback, {
    memorabilia_id :: non_neg_integer()
}).
-record(land_data_land_message_notification_data_notification_callback, {
    money :: non_neg_integer(),
    premium_currency :: non_neg_integer(),
    exp :: non_neg_integer(),
    source_length :: integer(),
    source :: string() | binary(),
    reason_length :: integer(),
    reason :: string() | binary(),
    building_id :: non_neg_integer(),
    character_id :: non_neg_integer(),
    position_x :: number(),
    position_y :: number(),
    position_z :: number()
}).
-record(land_data_land_message_inventory_item_data, {
    header :: land_data_land_message_entity_header(),
    item_type :: integer(),
    item_id :: integer(),
    count :: integer(),
    is_owner_list :: boolean()
}).

-type(land_data_land_message() :: #land_data_land_message{}).
-type(land_data_land_message_friend_data() :: #land_data_land_message_friend_data{}).
-type(land_data_land_message_entity_header() :: #land_data_land_message_entity_header{}).
-type(land_data_land_message_user_data() :: #land_data_land_message_user_data{}).
-type(land_data_land_message_user_data_saved_rating() :: #land_data_land_message_user_data_saved_rating{}).
-type(land_data_land_message_inner_land_data() :: #land_data_land_message_inner_land_data{}).
-type(land_data_land_message_terrain_data() :: #land_data_land_message_terrain_data{}).
-type(land_data_land_message_building_data() :: #land_data_land_message_building_data{}).
-type(land_data_land_message_building_data_owner_list() :: #land_data_land_message_building_data_owner_list{}).
-type(land_data_land_message_character_data() :: #land_data_land_message_character_data{}).
-type(land_data_land_message_consumable_data() :: #land_data_land_message_consumable_data{}).
-type(land_data_land_message_job_data() :: #land_data_land_message_job_data{}).
-type(land_data_land_message_quest_data() :: #land_data_land_message_quest_data{}).
-type(land_data_land_message_quest_data_objective_data() :: #land_data_land_message_quest_data_objective_data{}).
-type(land_data_land_message_quest_data_objective_data_building_custom_data() :: #land_data_land_message_quest_data_objective_data_building_custom_data{}).
-type(land_data_land_message_quest_data_objective_data_resources_custom_data() :: #land_data_land_message_quest_data_objective_data_resources_custom_data{}).
-type(land_data_land_message_quest_data_objective_data_item_count_custom_data() :: #land_data_land_message_quest_data_objective_data_item_count_custom_data{}).
-type(land_data_land_message_quest_data_objective_data_visit_friend_custom_data() :: #land_data_land_message_quest_data_objective_data_visit_friend_custom_data{}).
-type(land_data_land_message_notification_data() :: #land_data_land_message_notification_data{}).
-type(land_data_land_message_notification_data_consumable_callback() :: #land_data_land_message_notification_data_consumable_callback{}).
-type(land_data_land_message_notification_data_memorabilia_reward_callback() :: #land_data_land_message_notification_data_memorabilia_reward_callback{}).
-type(land_data_land_message_notification_data_notification_callback() :: #land_data_land_message_notification_data_notification_callback{}).
-type(land_data_land_message_inventory_item_data() :: #land_data_land_message_inventory_item_data{}).


-endif.
