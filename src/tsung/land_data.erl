-module(land_data).
-compile(export_all).

-include_lib("piqirun.hrl").
-include("land_data.hrl").

-spec gen_string/2 :: (Code :: piqirun_code(), X :: string() | binary()) -> iolist().
gen_string(Code, X) ->
    piqirun:string_to_block(Code, X).

-spec gen_proto_int32/2 :: (Code :: piqirun_code(), X :: integer()) -> iolist().
gen_proto_int32(Code, X) ->
    piqirun:integer_to_signed_varint(Code, X).


packed_gen_proto_int32(X) ->
    piqirun:integer_to_packed_signed_varint(X).

-spec gen_bool/2 :: (Code :: piqirun_code(), X :: boolean()) -> iolist().
gen_bool(Code, X) ->
    piqirun:boolean_to_varint(Code, X).


packed_gen_bool(X) ->
    piqirun:boolean_to_packed_varint(X).

-spec gen_uint32/2 :: (Code :: piqirun_code(), X :: non_neg_integer()) -> iolist().
gen_uint32(Code, X) ->
    piqirun:non_neg_integer_to_varint(Code, X).


packed_gen_uint32(X) ->
    piqirun:non_neg_integer_to_packed_varint(X).

-spec gen_uint64/2 :: (Code :: piqirun_code(), X :: non_neg_integer()) -> iolist().
gen_uint64(Code, X) ->
    piqirun:non_neg_integer_to_varint(Code, X).


packed_gen_uint64(X) ->
    piqirun:non_neg_integer_to_packed_varint(X).

-spec gen_binary/2 :: (Code :: piqirun_code(), X :: binary()) -> iolist().
gen_binary(Code, X) ->
    piqirun:binary_to_block(Code, X).

-spec gen_proto_int64/2 :: (Code :: piqirun_code(), X :: integer()) -> iolist().
gen_proto_int64(Code, X) ->
    piqirun:integer_to_signed_varint(Code, X).


packed_gen_proto_int64(X) ->
    piqirun:integer_to_packed_signed_varint(X).

-spec gen_float32/2 :: (Code :: piqirun_code(), X :: number()) -> iolist().
gen_float32(Code, X) ->
    piqirun:float_to_fixed32(Code, X).


packed_gen_float32(X) ->
    piqirun:float_to_packed_fixed32(X).

-spec gen_land_message/2 :: (Code :: piqirun_code(), X :: land_data_land_message()) -> iolist().
gen_land_message(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_optional_field(1, fun gen_string/2, X#land_data_land_message.id),
        piqirun:gen_optional_field(2, fun gen_land_message_friend_data/2, X#land_data_land_message.friend_data),
        piqirun:gen_optional_field(3, fun gen_land_message_user_data/2, X#land_data_land_message.user_data),
        piqirun:gen_optional_field(4, fun gen_land_message_inner_land_data/2, X#land_data_land_message.inner_land_data),
        piqirun:gen_optional_field(5, fun gen_land_message_terrain_data/2, X#land_data_land_message.roads_data),
        piqirun:gen_optional_field(6, fun gen_land_message_terrain_data/2, X#land_data_land_message.rivers_data),
        piqirun:gen_repeated_field(7, fun gen_land_message_building_data/2, X#land_data_land_message.building_data),
        piqirun:gen_repeated_field(8, fun gen_land_message_character_data/2, X#land_data_land_message.character_data),
        piqirun:gen_repeated_field(9, fun gen_land_message_consumable_data/2, X#land_data_land_message.consumable_data),
        piqirun:gen_repeated_field(10, fun gen_land_message_job_data/2, X#land_data_land_message.job_data),
        piqirun:gen_repeated_field(11, fun gen_land_message_quest_data/2, X#land_data_land_message.quest_data),
        piqirun:gen_repeated_field(12, fun gen_land_message_notification_data/2, X#land_data_land_message.notification_data),
        piqirun:gen_repeated_field(13, fun gen_land_message_inventory_item_data/2, X#land_data_land_message.inventory_item_data)
    ]).

-spec gen_land_message_friend_data/2 :: (Code :: piqirun_code(), X :: land_data_land_message_friend_data()) -> iolist().
gen_land_message_friend_data(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_optional_field(1, fun gen_proto_int32/2, X#land_data_land_message_friend_data.data_version),
        piqirun:gen_optional_field(2, fun gen_bool/2, X#land_data_land_message_friend_data.has_lemon_tree),
        piqirun:gen_optional_field(3, fun gen_proto_int32/2, X#land_data_land_message_friend_data.language),
        piqirun:gen_optional_field(4, fun gen_proto_int32/2, X#land_data_land_message_friend_data.level),
        piqirun:gen_optional_field(5, fun gen_string/2, X#land_data_land_message_friend_data.name),
        piqirun:gen_optional_field(6, fun gen_proto_int32/2, X#land_data_land_message_friend_data.rating)
    ]).

-spec gen_land_message_entity_header/2 :: (Code :: piqirun_code(), X :: land_data_land_message_entity_header()) -> iolist().
gen_land_message_entity_header(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_optional_field(1, fun gen_uint32/2, X#land_data_land_message_entity_header.id),
        piqirun:gen_optional_field(2, fun gen_land_message_entity_header_type/2, X#land_data_land_message_entity_header.type)
    ]).

-spec gen_land_message_entity_header_type/2 :: (Code :: piqirun_code(), X :: land_data_land_message_entity_header_type()) -> iolist().
gen_land_message_entity_header_type(Code, X) ->
    piqirun:integer_to_signed_varint(Code,
        case X of
            user -> 1;
        innerland -> 2;
        roads -> 3;
        rivers -> 4;
        building -> 5;
        character -> 6;
        consumable -> 7;
        job -> 8;
        quest -> 9;
        notification -> 10;
        inventory -> 11
        end
    ).


packed_gen_land_message_entity_header_type(X) ->
    piqirun:integer_to_packed_signed_varint(
        case X of
            user -> 1;
        innerland -> 2;
        roads -> 3;
        rivers -> 4;
        building -> 5;
        character -> 6;
        consumable -> 7;
        job -> 8;
        quest -> 9;
        notification -> 10;
        inventory -> 11
        end
    ).

-spec gen_land_message_user_data/2 :: (Code :: piqirun_code(), X :: land_data_land_message_user_data()) -> iolist().
gen_land_message_user_data(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_optional_field(1, fun gen_land_message_entity_header/2, X#land_data_land_message_user_data.header),
        piqirun:gen_optional_field(2, fun gen_uint64/2, X#land_data_land_message_user_data.last_bonus_collection),
        piqirun:gen_optional_field(3, fun gen_uint32/2, X#land_data_land_message_user_data.level),
        piqirun:gen_optional_field(4, fun gen_uint32/2, X#land_data_land_message_user_data.experience),
        piqirun:gen_optional_field(5, fun gen_uint32/2, X#land_data_land_message_user_data.money),
        piqirun:gen_optional_field(6, fun gen_uint32/2, X#land_data_land_message_user_data.premium_currency),
        piqirun:gen_optional_field(7, fun gen_uint32/2, X#land_data_land_message_user_data.entity_id),
        piqirun:gen_repeated_field(8, fun gen_land_message_user_data_saved_rating/2, X#land_data_land_message_user_data.saved_rating),
        piqirun:gen_optional_field(9, fun gen_binary/2, X#land_data_land_message_user_data.last_bonus),
        piqirun:gen_optional_field(10, fun gen_string/2, X#land_data_land_message_user_data.user_name),
        piqirun:gen_optional_field(11, fun gen_bool/2, X#land_data_land_message_user_data.friends_unlocked),
        piqirun:gen_optional_field(12, fun gen_bool/2, X#land_data_land_message_user_data.memorabilia_unlocked),
        piqirun:gen_optional_field(13, fun gen_bool/2, X#land_data_land_message_user_data.reorganize_unlocked),
        piqirun:gen_optional_field(14, fun gen_bool/2, X#land_data_land_message_user_data.first_purchase),
        piqirun:gen_optional_field(15, fun gen_bool/2, X#land_data_land_message_user_data.sidebar_default_open),
        piqirun:gen_optional_field(16, fun gen_bool/2, X#land_data_land_message_user_data.show_level_up)
    ]).

-spec gen_land_message_user_data_saved_rating/2 :: (Code :: piqirun_code(), X :: land_data_land_message_user_data_saved_rating()) -> iolist().
gen_land_message_user_data_saved_rating(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_optional_field(1, fun gen_uint32/2, X#land_data_land_message_user_data_saved_rating.saved_rating_elem_id),
        piqirun:gen_optional_field(2, fun gen_proto_int32/2, X#land_data_land_message_user_data_saved_rating.saved_rating_elem)
    ]).

-spec gen_land_message_inner_land_data/2 :: (Code :: piqirun_code(), X :: land_data_land_message_inner_land_data()) -> iolist().
gen_land_message_inner_land_data(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_optional_field(1, fun gen_land_message_entity_header/2, X#land_data_land_message_inner_land_data.header),
        piqirun:gen_optional_field(2, fun gen_string/2, X#land_data_land_message_inner_land_data.land_blocks),
        piqirun:gen_optional_field(3, fun gen_uint32/2, X#land_data_land_message_inner_land_data.next_instance_id),
        piqirun:gen_optional_field(4, fun gen_uint32/2, X#land_data_land_message_inner_land_data.num_chars),
        piqirun:gen_optional_field(5, fun gen_uint32/2, X#land_data_land_message_inner_land_data.num_buildings),
        piqirun:gen_optional_field(6, fun gen_uint32/2, X#land_data_land_message_inner_land_data.num_consumables),
        piqirun:gen_optional_field(7, fun gen_uint32/2, X#land_data_land_message_inner_land_data.num_jobs),
        piqirun:gen_optional_field(8, fun gen_uint32/2, X#land_data_land_message_inner_land_data.num_quests),
        piqirun:gen_optional_field(9, fun gen_uint32/2, X#land_data_land_message_inner_land_data.num_notices),
        piqirun:gen_optional_field(10, fun gen_uint32/2, X#land_data_land_message_inner_land_data.num_inventory_items),
        piqirun:gen_optional_field(11, fun gen_uint32/2, X#land_data_land_message_inner_land_data.num_memorabilia_items),
        piqirun:gen_optional_field(12, fun gen_uint32/2, X#land_data_land_message_inner_land_data.num_event_count_lists),
        piqirun:gen_optional_field(13, fun gen_uint32/2, X#land_data_land_message_inner_land_data.num_premium_unlocks),
        piqirun:gen_optional_field(14, fun gen_uint32/2, X#land_data_land_message_inner_land_data.num_action_limits),
        piqirun:gen_optional_field(15, fun gen_uint32/2, X#land_data_land_message_inner_land_data.num_runs_since_promotion),
        piqirun:gen_optional_field(16, fun gen_uint32/2, X#land_data_land_message_inner_land_data.level_of_rate_ask),
        piqirun:gen_optional_field(17, fun gen_proto_int64/2, X#land_data_land_message_inner_land_data.update_time),
        piqirun:gen_optional_field(18, fun gen_proto_int64/2, X#land_data_land_message_inner_land_data.time_spent_playing),
        piqirun:gen_optional_field(19, fun gen_proto_int64/2, X#land_data_land_message_inner_land_data.last_level_up_spent_time),
        piqirun:gen_optional_field(20, fun gen_proto_int64/2, X#land_data_land_message_inner_land_data.last_level_up_time_stamp),
        piqirun:gen_optional_field(21, fun gen_bool/2, X#land_data_land_message_inner_land_data.initial_save_done),
        piqirun:gen_optional_field(22, fun gen_proto_int64/2, X#land_data_land_message_inner_land_data.last_vandalism_feed_posted),
        piqirun:gen_optional_field(23, fun gen_proto_int64/2, X#land_data_land_message_inner_land_data.last_steal_building_feed_posted),
        piqirun:gen_optional_field(24, fun gen_proto_int64/2, X#land_data_land_message_inner_land_data.last_sideshow_bob_spawned),
        piqirun:gen_optional_field(25, fun gen_uint32/2, X#land_data_land_message_inner_land_data.next_currency_id)
    ]).

-spec gen_land_message_terrain_data/2 :: (Code :: piqirun_code(), X :: land_data_land_message_terrain_data()) -> iolist().
gen_land_message_terrain_data(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_optional_field(1, fun gen_land_message_entity_header/2, X#land_data_land_message_terrain_data.header),
        piqirun:gen_optional_field(2, fun gen_uint32/2, X#land_data_land_message_terrain_data.map_data_size),
        piqirun:gen_optional_field(3, fun gen_string/2, X#land_data_land_message_terrain_data.map_data)
    ]).

-spec gen_land_message_building_data/2 :: (Code :: piqirun_code(), X :: land_data_land_message_building_data()) -> iolist().
gen_land_message_building_data(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_optional_field(1, fun gen_land_message_entity_header/2, X#land_data_land_message_building_data.header),
        piqirun:gen_optional_field(2, fun gen_uint32/2, X#land_data_land_message_building_data.building),
        piqirun:gen_optional_field(3, fun gen_proto_int64/2, X#land_data_land_message_building_data.creation_time),
        piqirun:gen_optional_field(4, fun gen_proto_int64/2, X#land_data_land_message_building_data.update_time),
        piqirun:gen_optional_field(5, fun gen_float32/2, X#land_data_land_message_building_data.position_x),
        piqirun:gen_optional_field(6, fun gen_float32/2, X#land_data_land_message_building_data.position_y),
        piqirun:gen_optional_field(7, fun gen_bool/2, X#land_data_land_message_building_data.flipped),
        piqirun:gen_optional_field(8, fun gen_uint32/2, X#land_data_land_message_building_data.build_state),
        piqirun:gen_optional_field(9, fun gen_bool/2, X#land_data_land_message_building_data.hurry),
        piqirun:gen_optional_field(10, fun gen_uint32/2, X#land_data_land_message_building_data.crop_state),
        piqirun:gen_optional_field(11, fun gen_bool/2, X#land_data_land_message_building_data.bet_active),
        piqirun:gen_optional_field(12, fun gen_proto_int64/2, X#land_data_land_message_building_data.bet_start_time),
        piqirun:gen_optional_field(13, fun gen_uint32/2, X#land_data_land_message_building_data.bet_choice),
        piqirun:gen_optional_field(14, fun gen_bool/2, X#land_data_land_message_building_data.winning_bet),
        piqirun:gen_optional_field(15, fun gen_bool/2, X#land_data_land_message_building_data.vandalized),
        piqirun:gen_optional_field(16, fun gen_proto_int32/2, X#land_data_land_message_building_data.namelen),
        piqirun:gen_optional_field(17, fun gen_string/2, X#land_data_land_message_building_data.vandal_name),
        piqirun:gen_optional_field(18, fun gen_bool/2, X#land_data_land_message_building_data.is_owner_list),
        piqirun:gen_optional_field(19, fun gen_land_message_building_data_owner_list/2, X#land_data_land_message_building_data.owner_list)
    ]).

-spec gen_land_message_building_data_owner_list/2 :: (Code :: piqirun_code(), X :: land_data_land_message_building_data_owner_list()) -> iolist().
gen_land_message_building_data_owner_list(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_optional_field(1, fun gen_string/2, X#land_data_land_message_building_data_owner_list.id),
        piqirun:gen_repeated_field(2, fun gen_string/2, X#land_data_land_message_building_data_owner_list.user_name)
    ]).

-spec gen_land_message_character_data/2 :: (Code :: piqirun_code(), X :: land_data_land_message_character_data()) -> iolist().
gen_land_message_character_data(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_optional_field(1, fun gen_land_message_entity_header/2, X#land_data_land_message_character_data.header),
        piqirun:gen_optional_field(2, fun gen_uint32/2, X#land_data_land_message_character_data.character),
        piqirun:gen_optional_field(3, fun gen_proto_int64/2, X#land_data_land_message_character_data.update_time),
        piqirun:gen_optional_field(4, fun gen_float32/2, X#land_data_land_message_character_data.position_x),
        piqirun:gen_optional_field(5, fun gen_float32/2, X#land_data_land_message_character_data.position_y)
    ]).

-spec gen_land_message_consumable_data/2 :: (Code :: piqirun_code(), X :: land_data_land_message_consumable_data()) -> iolist().
gen_land_message_consumable_data(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_optional_field(1, fun gen_land_message_entity_header/2, X#land_data_land_message_consumable_data.header),
        piqirun:gen_optional_field(2, fun gen_uint32/2, X#land_data_land_message_consumable_data.consumable)
    ]).

-spec gen_land_message_job_data/2 :: (Code :: piqirun_code(), X :: land_data_land_message_job_data()) -> iolist().
gen_land_message_job_data(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_optional_field(1, fun gen_land_message_entity_header/2, X#land_data_land_message_job_data.header),
        piqirun:gen_optional_field(2, fun gen_uint32/2, X#land_data_land_message_job_data.job),
        piqirun:gen_optional_field(3, fun gen_uint32/2, X#land_data_land_message_job_data.char_ref),
        piqirun:gen_optional_field(4, fun gen_uint32/2, X#land_data_land_message_job_data.building_ref),
        piqirun:gen_optional_field(5, fun gen_proto_int64/2, X#land_data_land_message_job_data.update_time),
        piqirun:gen_optional_field(6, fun gen_proto_int32/2, X#land_data_land_message_job_data.state),
        piqirun:gen_optional_field(7, fun gen_bool/2, X#land_data_land_message_job_data.hurry),
        piqirun:gen_optional_field(8, fun gen_bool/2, X#land_data_land_message_job_data.has_building),
        piqirun:gen_optional_field(9, fun gen_bool/2, X#land_data_land_message_job_data.has_char)
    ]).

-spec gen_land_message_quest_data/2 :: (Code :: piqirun_code(), X :: land_data_land_message_quest_data()) -> iolist().
gen_land_message_quest_data(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_optional_field(1, fun gen_land_message_entity_header/2, X#land_data_land_message_quest_data.header),
        piqirun:gen_optional_field(2, fun gen_uint32/2, X#land_data_land_message_quest_data.quest_id),
        piqirun:gen_optional_field(3, fun gen_proto_int32/2, X#land_data_land_message_quest_data.quest_state),
        piqirun:gen_optional_field(4, fun gen_proto_int32/2, X#land_data_land_message_quest_data.quest_script_state),
        piqirun:gen_optional_field(5, fun gen_proto_int32/2, X#land_data_land_message_quest_data.num_objectives),
        piqirun:gen_repeated_field(6, fun gen_land_message_quest_data_objective_data/2, X#land_data_land_message_quest_data.objective_data)
    ]).

-spec gen_land_message_quest_data_objective_data/2 :: (Code :: piqirun_code(), X :: land_data_land_message_quest_data_objective_data()) -> iolist().
gen_land_message_quest_data_objective_data(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_optional_field(1, fun gen_proto_int32/2, X#land_data_land_message_quest_data_objective_data.objective_id),
        piqirun:gen_optional_field(2, fun gen_proto_int32/2, X#land_data_land_message_quest_data_objective_data.objective_state),
        piqirun:gen_optional_field(3, fun gen_proto_int32/2, X#land_data_land_message_quest_data_objective_data.objective_type),
        piqirun:gen_optional_field(4, fun gen_proto_int32/2, X#land_data_land_message_quest_data_objective_data.version),
        piqirun:gen_optional_field(5, fun gen_bool/2, X#land_data_land_message_quest_data_objective_data.quest_last_ret_val),
        piqirun:gen_optional_field(6, fun gen_bool/2, X#land_data_land_message_quest_data_objective_data.quest_local),
        piqirun:gen_optional_field(7, fun gen_bool/2, X#land_data_land_message_quest_data_objective_data.rushed),
        piqirun:gen_optional_field(8, fun gen_bool/2, X#land_data_land_message_quest_data_objective_data.keep_done),
        piqirun:gen_optional_field(9, fun gen_bool/2, X#land_data_land_message_quest_data_objective_data.block_done_script),
        piqirun:gen_optional_field(10, fun gen_land_message_quest_data_objective_data_building_custom_data/2, X#land_data_land_message_quest_data_objective_data.building_custom_data),
        piqirun:gen_optional_field(11, fun gen_land_message_quest_data_objective_data_resources_custom_data/2, X#land_data_land_message_quest_data_objective_data.resources_custom_data),
        piqirun:gen_optional_field(12, fun gen_land_message_quest_data_objective_data_item_count_custom_data/2, X#land_data_land_message_quest_data_objective_data.item_count_custom_data),
        piqirun:gen_optional_field(13, fun gen_land_message_quest_data_objective_data_visit_friend_custom_data/2, X#land_data_land_message_quest_data_objective_data.visit_friend_custom_data)
    ]).

-spec gen_land_message_quest_data_objective_data_building_custom_data/2 :: (Code :: piqirun_code(), X :: land_data_land_message_quest_data_objective_data_building_custom_data()) -> iolist().
gen_land_message_quest_data_objective_data_building_custom_data(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_optional_field(1, fun gen_proto_int32/2, X#land_data_land_message_quest_data_objective_data_building_custom_data.qty)
    ]).

-spec gen_land_message_quest_data_objective_data_resources_custom_data/2 :: (Code :: piqirun_code(), X :: land_data_land_message_quest_data_objective_data_resources_custom_data()) -> iolist().
gen_land_message_quest_data_objective_data_resources_custom_data(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_optional_field(1, fun gen_uint32/2, X#land_data_land_message_quest_data_objective_data_resources_custom_data.money),
        piqirun:gen_optional_field(2, fun gen_uint32/2, X#land_data_land_message_quest_data_objective_data_resources_custom_data.premium)
    ]).

-spec gen_land_message_quest_data_objective_data_item_count_custom_data/2 :: (Code :: piqirun_code(), X :: land_data_land_message_quest_data_objective_data_item_count_custom_data()) -> iolist().
gen_land_message_quest_data_objective_data_item_count_custom_data(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_optional_field(1, fun gen_proto_int32/2, X#land_data_land_message_quest_data_objective_data_item_count_custom_data.count)
    ]).

-spec gen_land_message_quest_data_objective_data_visit_friend_custom_data/2 :: (Code :: piqirun_code(), X :: land_data_land_message_quest_data_objective_data_visit_friend_custom_data()) -> iolist().
gen_land_message_quest_data_objective_data_visit_friend_custom_data(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_optional_field(1, fun gen_proto_int32/2, X#land_data_land_message_quest_data_objective_data_visit_friend_custom_data.num),
        piqirun:gen_optional_field(2, fun gen_bool/2, X#land_data_land_message_quest_data_objective_data_visit_friend_custom_data.fake_included),
        piqirun:gen_repeated_field(3, fun gen_string/2, X#land_data_land_message_quest_data_objective_data_visit_friend_custom_data.friend)
    ]).

-spec gen_land_message_notification_data/2 :: (Code :: piqirun_code(), X :: land_data_land_message_notification_data()) -> iolist().
gen_land_message_notification_data(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_optional_field(1, fun gen_land_message_entity_header/2, X#land_data_land_message_notification_data.header),
        piqirun:gen_optional_field(2, fun gen_uint32/2, X#land_data_land_message_notification_data.building_id),
        piqirun:gen_optional_field(3, fun gen_uint32/2, X#land_data_land_message_notification_data.character_id),
        piqirun:gen_optional_field(4, fun gen_uint32/2, X#land_data_land_message_notification_data.type),
        piqirun:gen_optional_field(5, fun gen_uint32/2, X#land_data_land_message_notification_data.callback_id),
        piqirun:gen_optional_field(6, fun gen_float32/2, X#land_data_land_message_notification_data.start_x),
        piqirun:gen_optional_field(7, fun gen_float32/2, X#land_data_land_message_notification_data.start_y),
        piqirun:gen_optional_field(8, fun gen_float32/2, X#land_data_land_message_notification_data.start_z),
        piqirun:gen_optional_field(9, fun gen_float32/2, X#land_data_land_message_notification_data.end_x),
        piqirun:gen_optional_field(10, fun gen_float32/2, X#land_data_land_message_notification_data.end_y),
        piqirun:gen_optional_field(11, fun gen_float32/2, X#land_data_land_message_notification_data.end_z),
        piqirun:gen_optional_field(12, fun gen_proto_int64/2, X#land_data_land_message_notification_data.move_time),
        piqirun:gen_optional_field(13, fun gen_bool/2, X#land_data_land_message_notification_data.is_static),
        piqirun:gen_optional_field(14, fun gen_bool/2, X#land_data_land_message_notification_data.is_setup),
        piqirun:gen_optional_field(15, fun gen_bool/2, X#land_data_land_message_notification_data.has_building),
        piqirun:gen_optional_field(16, fun gen_bool/2, X#land_data_land_message_notification_data.has_char),
        piqirun:gen_optional_field(17, fun gen_uint32/2, X#land_data_land_message_notification_data.history_type),
        piqirun:gen_optional_field(18, fun gen_uint32/2, X#land_data_land_message_notification_data.history_job_id),
        piqirun:gen_optional_field(19, fun gen_uint32/2, X#land_data_land_message_notification_data.history_building_id),
        piqirun:gen_optional_field(20, fun gen_uint32/2, X#land_data_land_message_notification_data.history_character_id),
        piqirun:gen_optional_field(21, fun gen_uint32/2, X#land_data_land_message_notification_data.history_quest_id),
        piqirun:gen_optional_field(22, fun gen_uint32/2, X#land_data_land_message_notification_data.history_building_instance_id),
        piqirun:gen_optional_field(23, fun gen_land_message_notification_data_consumable_callback/2, X#land_data_land_message_notification_data.consumable_callback),
        piqirun:gen_optional_field(24, fun gen_land_message_notification_data_memorabilia_reward_callback/2, X#land_data_land_message_notification_data.memorabilia_reward_callback),
        piqirun:gen_optional_field(25, fun gen_land_message_notification_data_notification_callback/2, X#land_data_land_message_notification_data.notification_callback)
    ]).

-spec gen_land_message_notification_data_consumable_callback/2 :: (Code :: piqirun_code(), X :: land_data_land_message_notification_data_consumable_callback()) -> iolist().
gen_land_message_notification_data_consumable_callback(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_optional_field(1, fun gen_uint32/2, X#land_data_land_message_notification_data_consumable_callback.consumable_id),
        piqirun:gen_optional_field(2, fun gen_proto_int32/2, X#land_data_land_message_notification_data_consumable_callback.source_length),
        piqirun:gen_optional_field(3, fun gen_string/2, X#land_data_land_message_notification_data_consumable_callback.source)
    ]).

-spec gen_land_message_notification_data_memorabilia_reward_callback/2 :: (Code :: piqirun_code(), X :: land_data_land_message_notification_data_memorabilia_reward_callback()) -> iolist().
gen_land_message_notification_data_memorabilia_reward_callback(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_optional_field(1, fun gen_uint32/2, X#land_data_land_message_notification_data_memorabilia_reward_callback.memorabilia_id)
    ]).

-spec gen_land_message_notification_data_notification_callback/2 :: (Code :: piqirun_code(), X :: land_data_land_message_notification_data_notification_callback()) -> iolist().
gen_land_message_notification_data_notification_callback(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_optional_field(1, fun gen_uint32/2, X#land_data_land_message_notification_data_notification_callback.money),
        piqirun:gen_optional_field(2, fun gen_uint32/2, X#land_data_land_message_notification_data_notification_callback.premium_currency),
        piqirun:gen_optional_field(3, fun gen_uint32/2, X#land_data_land_message_notification_data_notification_callback.exp),
        piqirun:gen_optional_field(4, fun gen_proto_int32/2, X#land_data_land_message_notification_data_notification_callback.source_length),
        piqirun:gen_optional_field(5, fun gen_string/2, X#land_data_land_message_notification_data_notification_callback.source),
        piqirun:gen_optional_field(6, fun gen_proto_int32/2, X#land_data_land_message_notification_data_notification_callback.reason_length),
        piqirun:gen_optional_field(7, fun gen_string/2, X#land_data_land_message_notification_data_notification_callback.reason),
        piqirun:gen_optional_field(8, fun gen_uint32/2, X#land_data_land_message_notification_data_notification_callback.building_id),
        piqirun:gen_optional_field(9, fun gen_uint32/2, X#land_data_land_message_notification_data_notification_callback.character_id),
        piqirun:gen_optional_field(10, fun gen_float32/2, X#land_data_land_message_notification_data_notification_callback.position_x),
        piqirun:gen_optional_field(11, fun gen_float32/2, X#land_data_land_message_notification_data_notification_callback.position_y),
        piqirun:gen_optional_field(12, fun gen_float32/2, X#land_data_land_message_notification_data_notification_callback.position_z)
    ]).

-spec gen_land_message_inventory_item_data/2 :: (Code :: piqirun_code(), X :: land_data_land_message_inventory_item_data()) -> iolist().
gen_land_message_inventory_item_data(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_optional_field(1, fun gen_land_message_entity_header/2, X#land_data_land_message_inventory_item_data.header),
        piqirun:gen_optional_field(2, fun gen_proto_int32/2, X#land_data_land_message_inventory_item_data.item_type),
        piqirun:gen_optional_field(3, fun gen_proto_int32/2, X#land_data_land_message_inventory_item_data.item_id),
        piqirun:gen_optional_field(4, fun gen_proto_int32/2, X#land_data_land_message_inventory_item_data.count),
        piqirun:gen_optional_field(5, fun gen_bool/2, X#land_data_land_message_inventory_item_data.is_owner_list)
    ]).

-spec gen_string/1 :: (X :: string() | binary()) -> iolist().
gen_string(X) ->
    gen_string('undefined', X).

-spec gen_proto_int32/1 :: (X :: integer()) -> iolist().
gen_proto_int32(X) ->
    gen_proto_int32('undefined', X).

-spec gen_bool/1 :: (X :: boolean()) -> iolist().
gen_bool(X) ->
    gen_bool('undefined', X).

-spec gen_uint32/1 :: (X :: non_neg_integer()) -> iolist().
gen_uint32(X) ->
    gen_uint32('undefined', X).

-spec gen_uint64/1 :: (X :: non_neg_integer()) -> iolist().
gen_uint64(X) ->
    gen_uint64('undefined', X).

-spec gen_binary/1 :: (X :: binary()) -> iolist().
gen_binary(X) ->
    gen_binary('undefined', X).

-spec gen_proto_int64/1 :: (X :: integer()) -> iolist().
gen_proto_int64(X) ->
    gen_proto_int64('undefined', X).

-spec gen_float32/1 :: (X :: number()) -> iolist().
gen_float32(X) ->
    gen_float32('undefined', X).

-spec gen_land_message/1 :: (X :: land_data_land_message()) -> iolist().
gen_land_message(X) ->
    gen_land_message('undefined', X).

-spec gen_land_message_friend_data/1 :: (X :: land_data_land_message_friend_data()) -> iolist().
gen_land_message_friend_data(X) ->
    gen_land_message_friend_data('undefined', X).

-spec gen_land_message_entity_header/1 :: (X :: land_data_land_message_entity_header()) -> iolist().
gen_land_message_entity_header(X) ->
    gen_land_message_entity_header('undefined', X).

-spec gen_land_message_entity_header_type/1 :: (X :: land_data_land_message_entity_header_type()) -> iolist().
gen_land_message_entity_header_type(X) ->
    gen_land_message_entity_header_type('undefined', X).

-spec gen_land_message_user_data/1 :: (X :: land_data_land_message_user_data()) -> iolist().
gen_land_message_user_data(X) ->
    gen_land_message_user_data('undefined', X).

-spec gen_land_message_user_data_saved_rating/1 :: (X :: land_data_land_message_user_data_saved_rating()) -> iolist().
gen_land_message_user_data_saved_rating(X) ->
    gen_land_message_user_data_saved_rating('undefined', X).

-spec gen_land_message_inner_land_data/1 :: (X :: land_data_land_message_inner_land_data()) -> iolist().
gen_land_message_inner_land_data(X) ->
    gen_land_message_inner_land_data('undefined', X).

-spec gen_land_message_terrain_data/1 :: (X :: land_data_land_message_terrain_data()) -> iolist().
gen_land_message_terrain_data(X) ->
    gen_land_message_terrain_data('undefined', X).

-spec gen_land_message_building_data/1 :: (X :: land_data_land_message_building_data()) -> iolist().
gen_land_message_building_data(X) ->
    gen_land_message_building_data('undefined', X).

-spec gen_land_message_building_data_owner_list/1 :: (X :: land_data_land_message_building_data_owner_list()) -> iolist().
gen_land_message_building_data_owner_list(X) ->
    gen_land_message_building_data_owner_list('undefined', X).

-spec gen_land_message_character_data/1 :: (X :: land_data_land_message_character_data()) -> iolist().
gen_land_message_character_data(X) ->
    gen_land_message_character_data('undefined', X).

-spec gen_land_message_consumable_data/1 :: (X :: land_data_land_message_consumable_data()) -> iolist().
gen_land_message_consumable_data(X) ->
    gen_land_message_consumable_data('undefined', X).

-spec gen_land_message_job_data/1 :: (X :: land_data_land_message_job_data()) -> iolist().
gen_land_message_job_data(X) ->
    gen_land_message_job_data('undefined', X).

-spec gen_land_message_quest_data/1 :: (X :: land_data_land_message_quest_data()) -> iolist().
gen_land_message_quest_data(X) ->
    gen_land_message_quest_data('undefined', X).

-spec gen_land_message_quest_data_objective_data/1 :: (X :: land_data_land_message_quest_data_objective_data()) -> iolist().
gen_land_message_quest_data_objective_data(X) ->
    gen_land_message_quest_data_objective_data('undefined', X).

-spec gen_land_message_quest_data_objective_data_building_custom_data/1 :: (X :: land_data_land_message_quest_data_objective_data_building_custom_data()) -> iolist().
gen_land_message_quest_data_objective_data_building_custom_data(X) ->
    gen_land_message_quest_data_objective_data_building_custom_data('undefined', X).

-spec gen_land_message_quest_data_objective_data_resources_custom_data/1 :: (X :: land_data_land_message_quest_data_objective_data_resources_custom_data()) -> iolist().
gen_land_message_quest_data_objective_data_resources_custom_data(X) ->
    gen_land_message_quest_data_objective_data_resources_custom_data('undefined', X).

-spec gen_land_message_quest_data_objective_data_item_count_custom_data/1 :: (X :: land_data_land_message_quest_data_objective_data_item_count_custom_data()) -> iolist().
gen_land_message_quest_data_objective_data_item_count_custom_data(X) ->
    gen_land_message_quest_data_objective_data_item_count_custom_data('undefined', X).

-spec gen_land_message_quest_data_objective_data_visit_friend_custom_data/1 :: (X :: land_data_land_message_quest_data_objective_data_visit_friend_custom_data()) -> iolist().
gen_land_message_quest_data_objective_data_visit_friend_custom_data(X) ->
    gen_land_message_quest_data_objective_data_visit_friend_custom_data('undefined', X).

-spec gen_land_message_notification_data/1 :: (X :: land_data_land_message_notification_data()) -> iolist().
gen_land_message_notification_data(X) ->
    gen_land_message_notification_data('undefined', X).

-spec gen_land_message_notification_data_consumable_callback/1 :: (X :: land_data_land_message_notification_data_consumable_callback()) -> iolist().
gen_land_message_notification_data_consumable_callback(X) ->
    gen_land_message_notification_data_consumable_callback('undefined', X).

-spec gen_land_message_notification_data_memorabilia_reward_callback/1 :: (X :: land_data_land_message_notification_data_memorabilia_reward_callback()) -> iolist().
gen_land_message_notification_data_memorabilia_reward_callback(X) ->
    gen_land_message_notification_data_memorabilia_reward_callback('undefined', X).

-spec gen_land_message_notification_data_notification_callback/1 :: (X :: land_data_land_message_notification_data_notification_callback()) -> iolist().
gen_land_message_notification_data_notification_callback(X) ->
    gen_land_message_notification_data_notification_callback('undefined', X).

-spec gen_land_message_inventory_item_data/1 :: (X :: land_data_land_message_inventory_item_data()) -> iolist().
gen_land_message_inventory_item_data(X) ->
    gen_land_message_inventory_item_data('undefined', X).

-spec parse_string/1 :: (X :: piqirun_buffer()) -> binary().
parse_string(X) ->
    piqirun:binary_string_of_block(X).

-spec parse_proto_int32/1 :: (X :: piqirun_buffer()) -> integer().
parse_proto_int32(X) ->
    piqirun:integer_of_signed_varint(X).


packed_parse_proto_int32(X) ->
    piqirun:integer_of_packed_signed_varint(X).

-spec parse_bool/1 :: (X :: piqirun_buffer()) -> boolean().
parse_bool(X) ->
    piqirun:boolean_of_varint(X).


packed_parse_bool(X) ->
    piqirun:boolean_of_packed_varint(X).

-spec parse_uint32/1 :: (X :: piqirun_buffer()) -> non_neg_integer().
parse_uint32(X) ->
    piqirun:non_neg_integer_of_varint(X).


packed_parse_uint32(X) ->
    piqirun:non_neg_integer_of_packed_varint(X).

-spec parse_uint64/1 :: (X :: piqirun_buffer()) -> non_neg_integer().
parse_uint64(X) ->
    piqirun:non_neg_integer_of_varint(X).


packed_parse_uint64(X) ->
    piqirun:non_neg_integer_of_packed_varint(X).

-spec parse_binary/1 :: (X :: piqirun_buffer()) -> binary().
parse_binary(X) ->
    piqirun:binary_of_block(X).

-spec parse_proto_int64/1 :: (X :: piqirun_buffer()) -> integer().
parse_proto_int64(X) ->
    piqirun:integer_of_signed_varint(X).


packed_parse_proto_int64(X) ->
    piqirun:integer_of_packed_signed_varint(X).

-spec parse_float32/1 :: (X :: piqirun_buffer()) -> float().
parse_float32(X) ->
    piqirun:float_of_fixed32(X).


packed_parse_float32(X) ->
    piqirun:float_of_packed_fixed32(X).

-spec parse_land_message/1 :: (X :: piqirun_buffer()) -> land_data_land_message().
parse_land_message(X) -> 
    R0 = piqirun:parse_record(X),
    {_Id, R1} = piqirun:parse_optional_field(1, fun parse_string/1, R0),
    {_Friend_data, R2} = piqirun:parse_optional_field(2, fun parse_land_message_friend_data/1, R1),
    {_User_data, R3} = piqirun:parse_optional_field(3, fun parse_land_message_user_data/1, R2),
    {_Inner_land_data, R4} = piqirun:parse_optional_field(4, fun parse_land_message_inner_land_data/1, R3),
    {_Roads_data, R5} = piqirun:parse_optional_field(5, fun parse_land_message_terrain_data/1, R4),
    {_Rivers_data, R6} = piqirun:parse_optional_field(6, fun parse_land_message_terrain_data/1, R5),
    {_Building_data, R7} = piqirun:parse_repeated_field(7, fun parse_land_message_building_data/1, R6),
    {_Character_data, R8} = piqirun:parse_repeated_field(8, fun parse_land_message_character_data/1, R7),
    {_Consumable_data, R9} = piqirun:parse_repeated_field(9, fun parse_land_message_consumable_data/1, R8),
    {_Job_data, R10} = piqirun:parse_repeated_field(10, fun parse_land_message_job_data/1, R9),
    {_Quest_data, R11} = piqirun:parse_repeated_field(11, fun parse_land_message_quest_data/1, R10),
    {_Notification_data, R12} = piqirun:parse_repeated_field(12, fun parse_land_message_notification_data/1, R11),
    {_Inventory_item_data, R13} = piqirun:parse_repeated_field(13, fun parse_land_message_inventory_item_data/1, R12),
    piqirun:check_unparsed_fields(R13),
    #land_data_land_message{
        id = _Id,
        friend_data = _Friend_data,
        user_data = _User_data,
        inner_land_data = _Inner_land_data,
        roads_data = _Roads_data,
        rivers_data = _Rivers_data,
        building_data = _Building_data,
        character_data = _Character_data,
        consumable_data = _Consumable_data,
        job_data = _Job_data,
        quest_data = _Quest_data,
        notification_data = _Notification_data,
        inventory_item_data = _Inventory_item_data
    }.

-spec parse_land_message_friend_data/1 :: (X :: piqirun_buffer()) -> land_data_land_message_friend_data().
parse_land_message_friend_data(X) -> 
    R0 = piqirun:parse_record(X),
    {_Data_version, R1} = piqirun:parse_optional_field(1, fun parse_proto_int32/1, R0),
    {_Has_lemon_tree, R2} = piqirun:parse_optional_field(2, fun parse_bool/1, R1),
    {_Language, R3} = piqirun:parse_optional_field(3, fun parse_proto_int32/1, R2),
    {_Level, R4} = piqirun:parse_optional_field(4, fun parse_proto_int32/1, R3),
    {_Name, R5} = piqirun:parse_optional_field(5, fun parse_string/1, R4),
    {_Rating, R6} = piqirun:parse_optional_field(6, fun parse_proto_int32/1, R5),
    piqirun:check_unparsed_fields(R6),
    #land_data_land_message_friend_data{
        data_version = _Data_version,
        has_lemon_tree = _Has_lemon_tree,
        language = _Language,
        level = _Level,
        name = _Name,
        rating = _Rating
    }.

-spec parse_land_message_entity_header/1 :: (X :: piqirun_buffer()) -> land_data_land_message_entity_header().
parse_land_message_entity_header(X) -> 
    R0 = piqirun:parse_record(X),
    {_Id, R1} = piqirun:parse_optional_field(1, fun parse_uint32/1, R0),
    {_Type, R2} = piqirun:parse_optional_field(2, fun parse_land_message_entity_header_type/1, R1),
    piqirun:check_unparsed_fields(R2),
    #land_data_land_message_entity_header{
        id = _Id,
        type = _Type
    }.

-spec parse_land_message_entity_header_type/1 :: (X :: piqirun_buffer()) -> land_data_land_message_entity_header_type().
parse_land_message_entity_header_type(X) ->
    case piqirun:integer_of_signed_varint(X) of
        1 -> user;
        2 -> innerland;
        3 -> roads;
        4 -> rivers;
        5 -> building;
        6 -> character;
        7 -> consumable;
        8 -> job;
        9 -> quest;
        10 -> notification;
        11 -> inventory;
        Y -> piqirun:error_enum_const(Y)
    end.


packed_parse_land_message_entity_header_type(X) ->
    {Code, Rest} = piqirun:integer_of_packed_signed_varint(X),
    {case Code of
        1 -> user;
        2 -> innerland;
        3 -> roads;
        4 -> rivers;
        5 -> building;
        6 -> character;
        7 -> consumable;
        8 -> job;
        9 -> quest;
        10 -> notification;
        11 -> inventory;
        Y -> piqirun:error_enum_const(Y)
    end, Rest}.

-spec parse_land_message_user_data/1 :: (X :: piqirun_buffer()) -> land_data_land_message_user_data().
parse_land_message_user_data(X) -> 
    R0 = piqirun:parse_record(X),
    {_Header, R1} = piqirun:parse_optional_field(1, fun parse_land_message_entity_header/1, R0),
    {_Last_bonus_collection, R2} = piqirun:parse_optional_field(2, fun parse_uint64/1, R1),
    {_Level, R3} = piqirun:parse_optional_field(3, fun parse_uint32/1, R2),
    {_Experience, R4} = piqirun:parse_optional_field(4, fun parse_uint32/1, R3),
    {_Money, R5} = piqirun:parse_optional_field(5, fun parse_uint32/1, R4),
    {_Premium_currency, R6} = piqirun:parse_optional_field(6, fun parse_uint32/1, R5),
    {_Entity_id, R7} = piqirun:parse_optional_field(7, fun parse_uint32/1, R6),
    {_Saved_rating, R8} = piqirun:parse_repeated_field(8, fun parse_land_message_user_data_saved_rating/1, R7),
    {_Last_bonus, R9} = piqirun:parse_optional_field(9, fun parse_binary/1, R8),
    {_User_name, R10} = piqirun:parse_optional_field(10, fun parse_string/1, R9),
    {_Friends_unlocked, R11} = piqirun:parse_optional_field(11, fun parse_bool/1, R10),
    {_Memorabilia_unlocked, R12} = piqirun:parse_optional_field(12, fun parse_bool/1, R11),
    {_Reorganize_unlocked, R13} = piqirun:parse_optional_field(13, fun parse_bool/1, R12),
    {_First_purchase, R14} = piqirun:parse_optional_field(14, fun parse_bool/1, R13),
    {_Sidebar_default_open, R15} = piqirun:parse_optional_field(15, fun parse_bool/1, R14),
    {_Show_level_up, R16} = piqirun:parse_optional_field(16, fun parse_bool/1, R15),
    piqirun:check_unparsed_fields(R16),
    #land_data_land_message_user_data{
        header = _Header,
        last_bonus_collection = _Last_bonus_collection,
        level = _Level,
        experience = _Experience,
        money = _Money,
        premium_currency = _Premium_currency,
        entity_id = _Entity_id,
        saved_rating = _Saved_rating,
        last_bonus = _Last_bonus,
        user_name = _User_name,
        friends_unlocked = _Friends_unlocked,
        memorabilia_unlocked = _Memorabilia_unlocked,
        reorganize_unlocked = _Reorganize_unlocked,
        first_purchase = _First_purchase,
        sidebar_default_open = _Sidebar_default_open,
        show_level_up = _Show_level_up
    }.

-spec parse_land_message_user_data_saved_rating/1 :: (X :: piqirun_buffer()) -> land_data_land_message_user_data_saved_rating().
parse_land_message_user_data_saved_rating(X) -> 
    R0 = piqirun:parse_record(X),
    {_Saved_rating_elem_id, R1} = piqirun:parse_optional_field(1, fun parse_uint32/1, R0),
    {_Saved_rating_elem, R2} = piqirun:parse_optional_field(2, fun parse_proto_int32/1, R1),
    piqirun:check_unparsed_fields(R2),
    #land_data_land_message_user_data_saved_rating{
        saved_rating_elem_id = _Saved_rating_elem_id,
        saved_rating_elem = _Saved_rating_elem
    }.

-spec parse_land_message_inner_land_data/1 :: (X :: piqirun_buffer()) -> land_data_land_message_inner_land_data().
parse_land_message_inner_land_data(X) -> 
    R0 = piqirun:parse_record(X),
    {_Header, R1} = piqirun:parse_optional_field(1, fun parse_land_message_entity_header/1, R0),
    {_Land_blocks, R2} = piqirun:parse_optional_field(2, fun parse_string/1, R1),
    {_Next_instance_id, R3} = piqirun:parse_optional_field(3, fun parse_uint32/1, R2),
    {_Num_chars, R4} = piqirun:parse_optional_field(4, fun parse_uint32/1, R3),
    {_Num_buildings, R5} = piqirun:parse_optional_field(5, fun parse_uint32/1, R4),
    {_Num_consumables, R6} = piqirun:parse_optional_field(6, fun parse_uint32/1, R5),
    {_Num_jobs, R7} = piqirun:parse_optional_field(7, fun parse_uint32/1, R6),
    {_Num_quests, R8} = piqirun:parse_optional_field(8, fun parse_uint32/1, R7),
    {_Num_notices, R9} = piqirun:parse_optional_field(9, fun parse_uint32/1, R8),
    {_Num_inventory_items, R10} = piqirun:parse_optional_field(10, fun parse_uint32/1, R9),
    {_Num_memorabilia_items, R11} = piqirun:parse_optional_field(11, fun parse_uint32/1, R10),
    {_Num_event_count_lists, R12} = piqirun:parse_optional_field(12, fun parse_uint32/1, R11),
    {_Num_premium_unlocks, R13} = piqirun:parse_optional_field(13, fun parse_uint32/1, R12),
    {_Num_action_limits, R14} = piqirun:parse_optional_field(14, fun parse_uint32/1, R13),
    {_Num_runs_since_promotion, R15} = piqirun:parse_optional_field(15, fun parse_uint32/1, R14),
    {_Level_of_rate_ask, R16} = piqirun:parse_optional_field(16, fun parse_uint32/1, R15),
    {_Update_time, R17} = piqirun:parse_optional_field(17, fun parse_proto_int64/1, R16),
    {_Time_spent_playing, R18} = piqirun:parse_optional_field(18, fun parse_proto_int64/1, R17),
    {_Last_level_up_spent_time, R19} = piqirun:parse_optional_field(19, fun parse_proto_int64/1, R18),
    {_Last_level_up_time_stamp, R20} = piqirun:parse_optional_field(20, fun parse_proto_int64/1, R19),
    {_Initial_save_done, R21} = piqirun:parse_optional_field(21, fun parse_bool/1, R20),
    {_Last_vandalism_feed_posted, R22} = piqirun:parse_optional_field(22, fun parse_proto_int64/1, R21),
    {_Last_steal_building_feed_posted, R23} = piqirun:parse_optional_field(23, fun parse_proto_int64/1, R22),
    {_Last_sideshow_bob_spawned, R24} = piqirun:parse_optional_field(24, fun parse_proto_int64/1, R23),
    {_Next_currency_id, R25} = piqirun:parse_optional_field(25, fun parse_uint32/1, R24),
    piqirun:check_unparsed_fields(R25),
    #land_data_land_message_inner_land_data{
        header = _Header,
        land_blocks = _Land_blocks,
        next_instance_id = _Next_instance_id,
        num_chars = _Num_chars,
        num_buildings = _Num_buildings,
        num_consumables = _Num_consumables,
        num_jobs = _Num_jobs,
        num_quests = _Num_quests,
        num_notices = _Num_notices,
        num_inventory_items = _Num_inventory_items,
        num_memorabilia_items = _Num_memorabilia_items,
        num_event_count_lists = _Num_event_count_lists,
        num_premium_unlocks = _Num_premium_unlocks,
        num_action_limits = _Num_action_limits,
        num_runs_since_promotion = _Num_runs_since_promotion,
        level_of_rate_ask = _Level_of_rate_ask,
        update_time = _Update_time,
        time_spent_playing = _Time_spent_playing,
        last_level_up_spent_time = _Last_level_up_spent_time,
        last_level_up_time_stamp = _Last_level_up_time_stamp,
        initial_save_done = _Initial_save_done,
        last_vandalism_feed_posted = _Last_vandalism_feed_posted,
        last_steal_building_feed_posted = _Last_steal_building_feed_posted,
        last_sideshow_bob_spawned = _Last_sideshow_bob_spawned,
        next_currency_id = _Next_currency_id
    }.

-spec parse_land_message_terrain_data/1 :: (X :: piqirun_buffer()) -> land_data_land_message_terrain_data().
parse_land_message_terrain_data(X) -> 
    R0 = piqirun:parse_record(X),
    {_Header, R1} = piqirun:parse_optional_field(1, fun parse_land_message_entity_header/1, R0),
    {_Map_data_size, R2} = piqirun:parse_optional_field(2, fun parse_uint32/1, R1),
    {_Map_data, R3} = piqirun:parse_optional_field(3, fun parse_string/1, R2),
    piqirun:check_unparsed_fields(R3),
    #land_data_land_message_terrain_data{
        header = _Header,
        map_data_size = _Map_data_size,
        map_data = _Map_data
    }.

-spec parse_land_message_building_data/1 :: (X :: piqirun_buffer()) -> land_data_land_message_building_data().
parse_land_message_building_data(X) -> 
    R0 = piqirun:parse_record(X),
    {_Header, R1} = piqirun:parse_optional_field(1, fun parse_land_message_entity_header/1, R0),
    {_Building, R2} = piqirun:parse_optional_field(2, fun parse_uint32/1, R1),
    {_Creation_time, R3} = piqirun:parse_optional_field(3, fun parse_proto_int64/1, R2),
    {_Update_time, R4} = piqirun:parse_optional_field(4, fun parse_proto_int64/1, R3),
    {_Position_x, R5} = piqirun:parse_optional_field(5, fun parse_float32/1, R4),
    {_Position_y, R6} = piqirun:parse_optional_field(6, fun parse_float32/1, R5),
    {_Flipped, R7} = piqirun:parse_optional_field(7, fun parse_bool/1, R6),
    {_Build_state, R8} = piqirun:parse_optional_field(8, fun parse_uint32/1, R7),
    {_Hurry, R9} = piqirun:parse_optional_field(9, fun parse_bool/1, R8),
    {_Crop_state, R10} = piqirun:parse_optional_field(10, fun parse_uint32/1, R9),
    {_Bet_active, R11} = piqirun:parse_optional_field(11, fun parse_bool/1, R10),
    {_Bet_start_time, R12} = piqirun:parse_optional_field(12, fun parse_proto_int64/1, R11),
    {_Bet_choice, R13} = piqirun:parse_optional_field(13, fun parse_uint32/1, R12),
    {_Winning_bet, R14} = piqirun:parse_optional_field(14, fun parse_bool/1, R13),
    {_Vandalized, R15} = piqirun:parse_optional_field(15, fun parse_bool/1, R14),
    {_Namelen, R16} = piqirun:parse_optional_field(16, fun parse_proto_int32/1, R15),
    {_Vandal_name, R17} = piqirun:parse_optional_field(17, fun parse_string/1, R16),
    {_Is_owner_list, R18} = piqirun:parse_optional_field(18, fun parse_bool/1, R17),
    {_Owner_list, R19} = piqirun:parse_optional_field(19, fun parse_land_message_building_data_owner_list/1, R18),
    piqirun:check_unparsed_fields(R19),
    #land_data_land_message_building_data{
        header = _Header,
        building = _Building,
        creation_time = _Creation_time,
        update_time = _Update_time,
        position_x = _Position_x,
        position_y = _Position_y,
        flipped = _Flipped,
        build_state = _Build_state,
        hurry = _Hurry,
        crop_state = _Crop_state,
        bet_active = _Bet_active,
        bet_start_time = _Bet_start_time,
        bet_choice = _Bet_choice,
        winning_bet = _Winning_bet,
        vandalized = _Vandalized,
        namelen = _Namelen,
        vandal_name = _Vandal_name,
        is_owner_list = _Is_owner_list,
        owner_list = _Owner_list
    }.

-spec parse_land_message_building_data_owner_list/1 :: (X :: piqirun_buffer()) -> land_data_land_message_building_data_owner_list().
parse_land_message_building_data_owner_list(X) -> 
    R0 = piqirun:parse_record(X),
    {_Id, R1} = piqirun:parse_optional_field(1, fun parse_string/1, R0),
    {_User_name, R2} = piqirun:parse_repeated_field(2, fun parse_string/1, R1),
    piqirun:check_unparsed_fields(R2),
    #land_data_land_message_building_data_owner_list{
        id = _Id,
        user_name = _User_name
    }.

-spec parse_land_message_character_data/1 :: (X :: piqirun_buffer()) -> land_data_land_message_character_data().
parse_land_message_character_data(X) -> 
    R0 = piqirun:parse_record(X),
    {_Header, R1} = piqirun:parse_optional_field(1, fun parse_land_message_entity_header/1, R0),
    {_Character, R2} = piqirun:parse_optional_field(2, fun parse_uint32/1, R1),
    {_Update_time, R3} = piqirun:parse_optional_field(3, fun parse_proto_int64/1, R2),
    {_Position_x, R4} = piqirun:parse_optional_field(4, fun parse_float32/1, R3),
    {_Position_y, R5} = piqirun:parse_optional_field(5, fun parse_float32/1, R4),
    piqirun:check_unparsed_fields(R5),
    #land_data_land_message_character_data{
        header = _Header,
        character = _Character,
        update_time = _Update_time,
        position_x = _Position_x,
        position_y = _Position_y
    }.

-spec parse_land_message_consumable_data/1 :: (X :: piqirun_buffer()) -> land_data_land_message_consumable_data().
parse_land_message_consumable_data(X) -> 
    R0 = piqirun:parse_record(X),
    {_Header, R1} = piqirun:parse_optional_field(1, fun parse_land_message_entity_header/1, R0),
    {_Consumable, R2} = piqirun:parse_optional_field(2, fun parse_uint32/1, R1),
    piqirun:check_unparsed_fields(R2),
    #land_data_land_message_consumable_data{
        header = _Header,
        consumable = _Consumable
    }.

-spec parse_land_message_job_data/1 :: (X :: piqirun_buffer()) -> land_data_land_message_job_data().
parse_land_message_job_data(X) -> 
    R0 = piqirun:parse_record(X),
    {_Header, R1} = piqirun:parse_optional_field(1, fun parse_land_message_entity_header/1, R0),
    {_Job, R2} = piqirun:parse_optional_field(2, fun parse_uint32/1, R1),
    {_Char_ref, R3} = piqirun:parse_optional_field(3, fun parse_uint32/1, R2),
    {_Building_ref, R4} = piqirun:parse_optional_field(4, fun parse_uint32/1, R3),
    {_Update_time, R5} = piqirun:parse_optional_field(5, fun parse_proto_int64/1, R4),
    {_State, R6} = piqirun:parse_optional_field(6, fun parse_proto_int32/1, R5),
    {_Hurry, R7} = piqirun:parse_optional_field(7, fun parse_bool/1, R6),
    {_Has_building, R8} = piqirun:parse_optional_field(8, fun parse_bool/1, R7),
    {_Has_char, R9} = piqirun:parse_optional_field(9, fun parse_bool/1, R8),
    piqirun:check_unparsed_fields(R9),
    #land_data_land_message_job_data{
        header = _Header,
        job = _Job,
        char_ref = _Char_ref,
        building_ref = _Building_ref,
        update_time = _Update_time,
        state = _State,
        hurry = _Hurry,
        has_building = _Has_building,
        has_char = _Has_char
    }.

-spec parse_land_message_quest_data/1 :: (X :: piqirun_buffer()) -> land_data_land_message_quest_data().
parse_land_message_quest_data(X) -> 
    R0 = piqirun:parse_record(X),
    {_Header, R1} = piqirun:parse_optional_field(1, fun parse_land_message_entity_header/1, R0),
    {_Quest_id, R2} = piqirun:parse_optional_field(2, fun parse_uint32/1, R1),
    {_Quest_state, R3} = piqirun:parse_optional_field(3, fun parse_proto_int32/1, R2),
    {_Quest_script_state, R4} = piqirun:parse_optional_field(4, fun parse_proto_int32/1, R3),
    {_Num_objectives, R5} = piqirun:parse_optional_field(5, fun parse_proto_int32/1, R4),
    {_Objective_data, R6} = piqirun:parse_repeated_field(6, fun parse_land_message_quest_data_objective_data/1, R5),
    piqirun:check_unparsed_fields(R6),
    #land_data_land_message_quest_data{
        header = _Header,
        quest_id = _Quest_id,
        quest_state = _Quest_state,
        quest_script_state = _Quest_script_state,
        num_objectives = _Num_objectives,
        objective_data = _Objective_data
    }.

-spec parse_land_message_quest_data_objective_data/1 :: (X :: piqirun_buffer()) -> land_data_land_message_quest_data_objective_data().
parse_land_message_quest_data_objective_data(X) -> 
    R0 = piqirun:parse_record(X),
    {_Objective_id, R1} = piqirun:parse_optional_field(1, fun parse_proto_int32/1, R0),
    {_Objective_state, R2} = piqirun:parse_optional_field(2, fun parse_proto_int32/1, R1),
    {_Objective_type, R3} = piqirun:parse_optional_field(3, fun parse_proto_int32/1, R2),
    {_Version, R4} = piqirun:parse_optional_field(4, fun parse_proto_int32/1, R3),
    {_Quest_last_ret_val, R5} = piqirun:parse_optional_field(5, fun parse_bool/1, R4),
    {_Quest_local, R6} = piqirun:parse_optional_field(6, fun parse_bool/1, R5),
    {_Rushed, R7} = piqirun:parse_optional_field(7, fun parse_bool/1, R6),
    {_Keep_done, R8} = piqirun:parse_optional_field(8, fun parse_bool/1, R7),
    {_Block_done_script, R9} = piqirun:parse_optional_field(9, fun parse_bool/1, R8),
    {_Building_custom_data, R10} = piqirun:parse_optional_field(10, fun parse_land_message_quest_data_objective_data_building_custom_data/1, R9),
    {_Resources_custom_data, R11} = piqirun:parse_optional_field(11, fun parse_land_message_quest_data_objective_data_resources_custom_data/1, R10),
    {_Item_count_custom_data, R12} = piqirun:parse_optional_field(12, fun parse_land_message_quest_data_objective_data_item_count_custom_data/1, R11),
    {_Visit_friend_custom_data, R13} = piqirun:parse_optional_field(13, fun parse_land_message_quest_data_objective_data_visit_friend_custom_data/1, R12),
    piqirun:check_unparsed_fields(R13),
    #land_data_land_message_quest_data_objective_data{
        objective_id = _Objective_id,
        objective_state = _Objective_state,
        objective_type = _Objective_type,
        version = _Version,
        quest_last_ret_val = _Quest_last_ret_val,
        quest_local = _Quest_local,
        rushed = _Rushed,
        keep_done = _Keep_done,
        block_done_script = _Block_done_script,
        building_custom_data = _Building_custom_data,
        resources_custom_data = _Resources_custom_data,
        item_count_custom_data = _Item_count_custom_data,
        visit_friend_custom_data = _Visit_friend_custom_data
    }.

-spec parse_land_message_quest_data_objective_data_building_custom_data/1 :: (X :: piqirun_buffer()) -> land_data_land_message_quest_data_objective_data_building_custom_data().
parse_land_message_quest_data_objective_data_building_custom_data(X) -> 
    R0 = piqirun:parse_record(X),
    {_Qty, R1} = piqirun:parse_optional_field(1, fun parse_proto_int32/1, R0),
    piqirun:check_unparsed_fields(R1),
    #land_data_land_message_quest_data_objective_data_building_custom_data{
        qty = _Qty
    }.

-spec parse_land_message_quest_data_objective_data_resources_custom_data/1 :: (X :: piqirun_buffer()) -> land_data_land_message_quest_data_objective_data_resources_custom_data().
parse_land_message_quest_data_objective_data_resources_custom_data(X) -> 
    R0 = piqirun:parse_record(X),
    {_Money, R1} = piqirun:parse_optional_field(1, fun parse_uint32/1, R0),
    {_Premium, R2} = piqirun:parse_optional_field(2, fun parse_uint32/1, R1),
    piqirun:check_unparsed_fields(R2),
    #land_data_land_message_quest_data_objective_data_resources_custom_data{
        money = _Money,
        premium = _Premium
    }.

-spec parse_land_message_quest_data_objective_data_item_count_custom_data/1 :: (X :: piqirun_buffer()) -> land_data_land_message_quest_data_objective_data_item_count_custom_data().
parse_land_message_quest_data_objective_data_item_count_custom_data(X) -> 
    R0 = piqirun:parse_record(X),
    {_Count, R1} = piqirun:parse_optional_field(1, fun parse_proto_int32/1, R0),
    piqirun:check_unparsed_fields(R1),
    #land_data_land_message_quest_data_objective_data_item_count_custom_data{
        count = _Count
    }.

-spec parse_land_message_quest_data_objective_data_visit_friend_custom_data/1 :: (X :: piqirun_buffer()) -> land_data_land_message_quest_data_objective_data_visit_friend_custom_data().
parse_land_message_quest_data_objective_data_visit_friend_custom_data(X) -> 
    R0 = piqirun:parse_record(X),
    {_Num, R1} = piqirun:parse_optional_field(1, fun parse_proto_int32/1, R0),
    {_Fake_included, R2} = piqirun:parse_optional_field(2, fun parse_bool/1, R1),
    {_Friend, R3} = piqirun:parse_repeated_field(3, fun parse_string/1, R2),
    piqirun:check_unparsed_fields(R3),
    #land_data_land_message_quest_data_objective_data_visit_friend_custom_data{
        num = _Num,
        fake_included = _Fake_included,
        friend = _Friend
    }.

-spec parse_land_message_notification_data/1 :: (X :: piqirun_buffer()) -> land_data_land_message_notification_data().
parse_land_message_notification_data(X) -> 
    R0 = piqirun:parse_record(X),
    {_Header, R1} = piqirun:parse_optional_field(1, fun parse_land_message_entity_header/1, R0),
    {_Building_id, R2} = piqirun:parse_optional_field(2, fun parse_uint32/1, R1),
    {_Character_id, R3} = piqirun:parse_optional_field(3, fun parse_uint32/1, R2),
    {_Type, R4} = piqirun:parse_optional_field(4, fun parse_uint32/1, R3),
    {_Callback_id, R5} = piqirun:parse_optional_field(5, fun parse_uint32/1, R4),
    {_Start_x, R6} = piqirun:parse_optional_field(6, fun parse_float32/1, R5),
    {_Start_y, R7} = piqirun:parse_optional_field(7, fun parse_float32/1, R6),
    {_Start_z, R8} = piqirun:parse_optional_field(8, fun parse_float32/1, R7),
    {_End_x, R9} = piqirun:parse_optional_field(9, fun parse_float32/1, R8),
    {_End_y, R10} = piqirun:parse_optional_field(10, fun parse_float32/1, R9),
    {_End_z, R11} = piqirun:parse_optional_field(11, fun parse_float32/1, R10),
    {_Move_time, R12} = piqirun:parse_optional_field(12, fun parse_proto_int64/1, R11),
    {_Is_static, R13} = piqirun:parse_optional_field(13, fun parse_bool/1, R12),
    {_Is_setup, R14} = piqirun:parse_optional_field(14, fun parse_bool/1, R13),
    {_Has_building, R15} = piqirun:parse_optional_field(15, fun parse_bool/1, R14),
    {_Has_char, R16} = piqirun:parse_optional_field(16, fun parse_bool/1, R15),
    {_History_type, R17} = piqirun:parse_optional_field(17, fun parse_uint32/1, R16),
    {_History_job_id, R18} = piqirun:parse_optional_field(18, fun parse_uint32/1, R17),
    {_History_building_id, R19} = piqirun:parse_optional_field(19, fun parse_uint32/1, R18),
    {_History_character_id, R20} = piqirun:parse_optional_field(20, fun parse_uint32/1, R19),
    {_History_quest_id, R21} = piqirun:parse_optional_field(21, fun parse_uint32/1, R20),
    {_History_building_instance_id, R22} = piqirun:parse_optional_field(22, fun parse_uint32/1, R21),
    {_Consumable_callback, R23} = piqirun:parse_optional_field(23, fun parse_land_message_notification_data_consumable_callback/1, R22),
    {_Memorabilia_reward_callback, R24} = piqirun:parse_optional_field(24, fun parse_land_message_notification_data_memorabilia_reward_callback/1, R23),
    {_Notification_callback, R25} = piqirun:parse_optional_field(25, fun parse_land_message_notification_data_notification_callback/1, R24),
    piqirun:check_unparsed_fields(R25),
    #land_data_land_message_notification_data{
        header = _Header,
        building_id = _Building_id,
        character_id = _Character_id,
        type = _Type,
        callback_id = _Callback_id,
        start_x = _Start_x,
        start_y = _Start_y,
        start_z = _Start_z,
        end_x = _End_x,
        end_y = _End_y,
        end_z = _End_z,
        move_time = _Move_time,
        is_static = _Is_static,
        is_setup = _Is_setup,
        has_building = _Has_building,
        has_char = _Has_char,
        history_type = _History_type,
        history_job_id = _History_job_id,
        history_building_id = _History_building_id,
        history_character_id = _History_character_id,
        history_quest_id = _History_quest_id,
        history_building_instance_id = _History_building_instance_id,
        consumable_callback = _Consumable_callback,
        memorabilia_reward_callback = _Memorabilia_reward_callback,
        notification_callback = _Notification_callback
    }.

-spec parse_land_message_notification_data_consumable_callback/1 :: (X :: piqirun_buffer()) -> land_data_land_message_notification_data_consumable_callback().
parse_land_message_notification_data_consumable_callback(X) -> 
    R0 = piqirun:parse_record(X),
    {_Consumable_id, R1} = piqirun:parse_optional_field(1, fun parse_uint32/1, R0),
    {_Source_length, R2} = piqirun:parse_optional_field(2, fun parse_proto_int32/1, R1),
    {_Source, R3} = piqirun:parse_optional_field(3, fun parse_string/1, R2),
    piqirun:check_unparsed_fields(R3),
    #land_data_land_message_notification_data_consumable_callback{
        consumable_id = _Consumable_id,
        source_length = _Source_length,
        source = _Source
    }.

-spec parse_land_message_notification_data_memorabilia_reward_callback/1 :: (X :: piqirun_buffer()) -> land_data_land_message_notification_data_memorabilia_reward_callback().
parse_land_message_notification_data_memorabilia_reward_callback(X) -> 
    R0 = piqirun:parse_record(X),
    {_Memorabilia_id, R1} = piqirun:parse_optional_field(1, fun parse_uint32/1, R0),
    piqirun:check_unparsed_fields(R1),
    #land_data_land_message_notification_data_memorabilia_reward_callback{
        memorabilia_id = _Memorabilia_id
    }.

-spec parse_land_message_notification_data_notification_callback/1 :: (X :: piqirun_buffer()) -> land_data_land_message_notification_data_notification_callback().
parse_land_message_notification_data_notification_callback(X) -> 
    R0 = piqirun:parse_record(X),
    {_Money, R1} = piqirun:parse_optional_field(1, fun parse_uint32/1, R0),
    {_Premium_currency, R2} = piqirun:parse_optional_field(2, fun parse_uint32/1, R1),
    {_Exp, R3} = piqirun:parse_optional_field(3, fun parse_uint32/1, R2),
    {_Source_length, R4} = piqirun:parse_optional_field(4, fun parse_proto_int32/1, R3),
    {_Source, R5} = piqirun:parse_optional_field(5, fun parse_string/1, R4),
    {_Reason_length, R6} = piqirun:parse_optional_field(6, fun parse_proto_int32/1, R5),
    {_Reason, R7} = piqirun:parse_optional_field(7, fun parse_string/1, R6),
    {_Building_id, R8} = piqirun:parse_optional_field(8, fun parse_uint32/1, R7),
    {_Character_id, R9} = piqirun:parse_optional_field(9, fun parse_uint32/1, R8),
    {_Position_x, R10} = piqirun:parse_optional_field(10, fun parse_float32/1, R9),
    {_Position_y, R11} = piqirun:parse_optional_field(11, fun parse_float32/1, R10),
    {_Position_z, R12} = piqirun:parse_optional_field(12, fun parse_float32/1, R11),
    piqirun:check_unparsed_fields(R12),
    #land_data_land_message_notification_data_notification_callback{
        money = _Money,
        premium_currency = _Premium_currency,
        exp = _Exp,
        source_length = _Source_length,
        source = _Source,
        reason_length = _Reason_length,
        reason = _Reason,
        building_id = _Building_id,
        character_id = _Character_id,
        position_x = _Position_x,
        position_y = _Position_y,
        position_z = _Position_z
    }.

-spec parse_land_message_inventory_item_data/1 :: (X :: piqirun_buffer()) -> land_data_land_message_inventory_item_data().
parse_land_message_inventory_item_data(X) -> 
    R0 = piqirun:parse_record(X),
    {_Header, R1} = piqirun:parse_optional_field(1, fun parse_land_message_entity_header/1, R0),
    {_Item_type, R2} = piqirun:parse_optional_field(2, fun parse_proto_int32/1, R1),
    {_Item_id, R3} = piqirun:parse_optional_field(3, fun parse_proto_int32/1, R2),
    {_Count, R4} = piqirun:parse_optional_field(4, fun parse_proto_int32/1, R3),
    {_Is_owner_list, R5} = piqirun:parse_optional_field(5, fun parse_bool/1, R4),
    piqirun:check_unparsed_fields(R5),
    #land_data_land_message_inventory_item_data{
        header = _Header,
        item_type = _Item_type,
        item_id = _Item_id,
        count = _Count,
        is_owner_list = _Is_owner_list
    }.



