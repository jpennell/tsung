-module(simp_land).
-export([
    %% traffic pattern
    get_traffic_pattern/1,
    get_v2_traffic_pattern/1,

    generate_post_land/1,

    %% V2 client-specific
    generate_v2_post_land/1,
    generate_v2_whole_land_update/1
    ]).

%% Returns traffic distribution controls
get_traffic_pattern({_, _}) ->
    %% scale up so we're comparing integers, can go to 1/100 of a %
    ScaleUp = 100,

    CreateUser = do_traffic_calc(ScaleUp, 0.01),
    GetFriendData = do_traffic_calc(ScaleUp, 3),
    GetCurrency = do_traffic_calc(ScaleUp, 12),
    MigrateLand = do_traffic_calc(ScaleUp, 0.03),
    UserNotificationStatus = do_traffic_calc(ScaleUp, 25),
    Time_ClientConfig = do_traffic_calc(ScaleUp, 4),
    Sysstat = do_traffic_calc(ScaleUp, 0.1),

    [CreateUser, GetFriendData, GetCurrency, MigrateLand, UserNotificationStatus, Time_ClientConfig, Sysstat].

%% Returns traffic distribution controls
get_v2_traffic_pattern({_,_}) ->
    ScaleUp = 100, %% can go to 1/100 of a %

    CreateUser = do_traffic_calc(ScaleUp, 0.01),
    GetFriendData = do_traffic_calc(ScaleUp, 10),
    GetCurrency = do_traffic_calc(ScaleUp, 10),
    MigrateLand = do_traffic_calc(ScaleUp, 0.03),
    UserNotificationStatus = do_traffic_calc(ScaleUp, 8),
    GetLandOld = do_traffic_calc(ScaleUp, 1),

    [CreateUser, GetFriendData, GetCurrency, MigrateLand, UserNotificationStatus, GetLandOld].

%% takes in a percentage and determines if the REST call should be made
do_traffic_calc(ScaleUp, Percentage) ->
    Random = random:uniform() * 100 * ScaleUp,
    Compare = round(Percentage * ScaleUp),

    if
        Random =< Compare ->
            Return = 1;
        true ->
            Return = 0
    end,

    Return.

%% Returns the current time in milleseconds
current_time_millis() ->
    {MegaSec, Sec, MicroSec} = now(),
    MegaSec*1000000000 + Sec*1000 + MicroSec div 1000.

%% Loads the post land type from the flat file
load_land_data(LandType, {_,DynVars}) ->
    case ts_dynvars:lookup(list_to_atom(LandType),DynVars) of
        {ok, RawLand} ->
            Sland = RawLand;
        false ->
            {ok, RawLand} = file:read_file("lands/" ++ LandType ++ ".simp"),
            Sland = binary:bin_to_list(RawLand),
            ts_dynvars:set(list_to_atom(LandType),Sland,DynVars)
    end,

    Sland.

%% Recursively replaces tokens in a string
%% Format is a String, then a list of old-new pairs
replace(String, [{Old, New}|T]) when is_integer(New) ->
    replace(String, [{Old, integer_to_list(New)}|T]);
replace(String, [{Old, New}|T]) when is_binary(New) ->
    replace(String, [{Old, binary:bin_to_list(New)}|T]);
replace(String, [{Old, New}|T]) ->
    replace(strre:gsub(String, Old, New), T);
replace(String, []) ->
    String.

%% Functions that construct an XML String out of Tsung representations of XML elements
construct_xml([{Tagname, Tagattributes, Tagsubdata}|T]) ->
    "<" ++ binary:bin_to_list(Tagname) ++ add_attributes(Tagattributes) ++ ">" ++
        "" ++ construct_xml(Tagsubdata) ++
    "</" ++ binary:bin_to_list(Tagname) ++ ">
    " ++ construct_xml(T);
construct_xml([]) ->
    "".
add_attributes([{Name,Value}|T]) ->
    " " ++ binary:bin_to_list(Name) ++ "=\"" ++ binary:bin_to_list(Value) ++ "\"" ++ add_attributes(T);
add_attributes([]) ->
    "".

%% END HELPER FUNCTIONS


%% POST LAND/CHARACTER
land_character({Pid,DynVars}) ->
    {ok,CharacterId} = ts_dynvars:lookup(characterId,DynVars),

    if
        CharacterId == "" ->
            "";
        CharacterId =/= "" ->
            Sland = load_land_data("land_character", {Pid,DynVars}),
            Millis = current_time_millis(),
            {ok,EntityId} = ts_dynvars:lookup(characterEntityId,DynVars),

            replace(Sland, [
                {"##TIME##", Millis},
                {"##CHARACTER_ID##", CharacterId},
                {"##ENTITY_ID##", EntityId}
            ])
    end.

%% POST LAND/INNERLAND
land_innerland({Pid,DynVars}) ->
    {ok,InnerLandId} = ts_dynvars:lookup(innerlandId,DynVars),

    if
        InnerLandId == "" ->
            "";
        InnerLandId =/= "" ->
            Sland = load_land_data("land_innerland", {Pid,DynVars}),
            CurrentMillis = current_time_millis(),
            LastMillis = CurrentMillis - 10000,

            replace(Sland, [
                {"##INNERLAND_ID##", InnerLandId},
                {"##CURRENT_TIME##", CurrentMillis},
                {"##LAST_TIME##", LastMillis}
            ])
    end.

%% POST LAND/USER
land_user({Pid,DynVars}) ->
    {ok,UserId} = ts_dynvars:lookup(userEntityId,DynVars),

    if
        UserId == "" ->
            "";
        UserId =/= "" ->
            Sland = load_land_data("land_user", {Pid,DynVars}),
            {ok,Xp} = ts_dynvars:lookup(userXp,DynVars),
            {ok,Bonus} = ts_dynvars:lookup(userBonus,DynVars),
            {ok,Level} = ts_dynvars:lookup(userLevel,DynVars),
            {ok,Money} = ts_dynvars:lookup(userMoney,DynVars),

            replace(Sland, [
                {"##USER_ENTITY_ID##", UserId},
                {"##USER_XP##", Xp},
                {"##USER_BONUS##", Bonus},
                {"##USER_LEVEL##", Level},
                {"##USER_MONEY##", Money}
            ])
    end.

%% POST LAND/LAND
land_land({Pid, DynVars}) ->
    Sland = load_land_data("land_land", {Pid, DynVars}),

    {ok, FriendRating} = ts_dynvars:lookup(friendRating, DynVars),
    {ok, UserId} = ts_dynvars:lookup(userId, DynVars),
    {ok, Level} = ts_dynvars:lookup(userLevel, DynVars),

    replace(Sland, [
        {"##USER_ID##", UserId},
        {"##RATING##", FriendRating},
        {"##LEVEL##", Level}
    ]).

%% POST LAND/CURRENCY
land_currency({Pid, DynVars}) ->
    Sland = load_land_data("land_currency", {Pid, DynVars}),

    {ok, UserId} = ts_dynvars:lookup(userId, DynVars),

    replace(Sland, [
        {"##USER_ID##", UserId}
    ]).

%% POST LAND/SIDEBAR
land_sidebar({_, DynVars}) ->
    {ok, SidebarId} = ts_dynvars:lookup(sidebarId, DynVars),

    if
        SidebarId == "" ->
            "";
        SidebarId =/= "" ->
            {ok, NumElements} = ts_dynvars:lookup(sidebarNumElements, DynVars),
            {ok, IdentifierList} = ts_dynvars:lookup(sidebarIdetifierList, DynVars),
            {ok, StateList} = ts_dynvars:lookup(sidebarStateList, DynVars),

            "<Message operation=\"POST\" id=\"" ++ SidebarId ++ "\" type=\"LAND/SIDEBAR\">
                <Entity>
                    <EntityData>
                        <Sidebar numSidebarElements=\"" ++ NumElements ++ "\">" ++
                            construct_sidebar(IdentifierList, StateList) ++
                        "</Sidebar>
                    </EntityData>
                </Entity>
            </Message>"
    end.

%% HELPER FOR POST LAND/SIDEBAR
construct_sidebar([Identifier|T1], [State|T2]) ->
    "<SidebarElement type=\"0\" identifier=\"" ++ binary:bin_to_list(Identifier) ++
        "\" state=\"" ++ binary:bin_to_list(State) ++ "\" updateTime=\"" ++
        integer_to_list(current_time_millis()) ++ "\"/>\n" ++ construct_sidebar(T1, T2);
construct_sidebar([],[]) ->
    "".

%% POST LAND/QUEST
land_post_quest({_, DynVars}) ->
    {ok, EntityIdList} = ts_dynvars:lookup(questEntityIdList, DynVars),
    {ok, EntityData} = ts_dynvars:lookup(questEntityList, DynVars),

    construct_entities("LAND/QUEST", "POST", EntityIdList, EntityData).

%% HELPER FOR POST LAND/QUEST
construct_entities(Type, Method, [EntityId|T1], [EntityData|T2]) ->
    "<Message operation=\"" ++ Method ++ "\" id=\"" ++ binary:bin_to_list(EntityId) ++ "\" type=\"" ++ Type ++ "\">
        <Entity>
        " ++ construct_xml([EntityData]) ++
        "
        </Entity>
    </Message>
    " ++ construct_entities(Type, Method, T1, T2);
construct_entities(_, _, [],[]) -> %% was Type, Method
    "".

%% PUT LAND/QUEST
land_put_quest({Pid, DynVars}) ->
    Sland = load_land_data("land_quest", {Pid, DynVars}),
    EntityId = 1000000 + random:uniform(1000000),
    QuestId = 1000000 + random:uniform(1000000),

    replace(Sland, [
        {"##ENTITY_ID##", EntityId},
        {"##QUEST_ID##", QuestId}
    ]).

%% PUT LAND/JOB
land_put_job({Pid,DynVars}) ->
    Sland = load_land_data("land_job", {Pid, DynVars}),

    {ok, BuildingId} = ts_dynvars:lookup(buildingTypeId, DynVars),
    EntityId = 1000000 + random:uniform(1000000),
    JobId = 1000000 + random:uniform(1000000),

    replace(Sland, [
        {"##METHOD##", "PUT"},
        {"##BUILDING_ID##", BuildingId},
        {"##JOB_ENTITY_ID##", EntityId},
        {"##JOB_ID##", JobId},
        {"##TIME##", current_time_millis()},
        {"##STATE##", "0"}
    ]).

%% POST LAND/JOB
land_post_job({Pid, DynVars}) ->
    Sland = load_land_data("land_job", {Pid,DynVars}),

    {ok, EntityId} = ts_dynvars:lookup(jobEntityId, DynVars),
    {ok, BuildingRef} = ts_dynvars:lookup(jobBuildingRef, DynVars),
    {ok, JobId} = ts_dynvars:lookup(jobId, DynVars),

    if
        EntityId =/= "" ->
            replace(Sland, [
                {"##METHOD##", "POST"},
                {"##JOB_ENTITY_ID##", EntityId},
                {"##JOB_ID##", JobId},
                {"##BUILDING_ID##", BuildingRef},
                {"##TIME##", current_time_millis()},
                {"##STATE##", "1"}
            ]);
        EntityId == "" ->
            ""
    end.

%% DELETE LAND/JOB
land_delete_job({_, DynVars}) ->
    {ok, JobId} = ts_dynvars:lookup(jobId, DynVars),

    if
        JobId =/= "" ->
            "<Message operation=\"DELETE\" id=\"" ++ JobId ++ "\" type=\"LAND/JOB\" />";
        JobId == "" ->
            ""
    end.

%% PUT LAND/NOTICE
land_put_notice({Pid, DynVars}) ->
    Sland = load_land_data("land_notice", {Pid, DynVars}),

    {ok, BuildingId} = ts_dynvars:lookup(buildingTypeId, DynVars),

    EntityId = 1000000 + random:uniform(1000000),
    replace(Sland, [
        {"##NOTICE_ENTITY_ID##", EntityId},
        {"##BUILDING_ID##", BuildingId}
    ]).

%% POST LAND/NOTICE
land_post_notice({_, DynVars}) ->
    {ok, NoticeId} = ts_dynvars:lookup(noticeId, DynVars),
    {ok, EntityData} = ts_dynvars:lookup(noticeData, DynVars),

    construct_entities("LAND/NOTICE", "POST", NoticeId, EntityData).

%% DELETE LAND/NOTICE
land_delete_notice({_, DynVars}) ->
    {ok, NoticeId} = ts_dynvars:lookup(noticeId, DynVars),

    if
        NoticeId =/= "" ->
            "<Message operation=\"DELETE\" id=\"" ++ NoticeId ++ "\" type=\"LAND/NOTICE\" />";
        NoticeId == "" ->
            ""
    end.

%% PUT NOTIFICATION
%% PUT LAND/PUSHNOTIFICATION
land_put_notification({_, DynVars}) ->
    {ok, NotificationEntityId} = ts_dynvars:lookup(notificationEntityId, DynVars),
    {ok, ReadNotificationPushId} = ts_dynvars:lookup(notificationPushId, DynVars),
    {ok, UserId} = ts_dynvars:lookup(userId, DynVars),

    %% If we read a notification, use the ID.  If we didn't, generate a new 40-ish digit one
    if
        ReadNotificationPushId == "" ->
            NotificationPushId = integer_to_list(1000000000 + random:uniform(9000000000)) ++ integer_to_list(1000000000 + random:uniform(9000000000)) ++
                        integer_to_list(1000000000 + random:uniform(9000000000)) ++ integer_to_list(1000000000 + random:uniform(9000000000));
        ReadNotificationPushId =/= "" ->
            NotificationPushId = ReadNotificationPushId
    end,

    Message1 = "<Message operation=\"PUT\" type=\"NOTIFICATION\">
        <Notification id=\"" ++ NotificationPushId ++ "\" toPlayer=\"" ++ binary:bin_to_list(UserId) ++ "\"
        scheduledIn=\"259200\" templateName=\"thesimpsonstappedout_push_ComeBack3Dat\">&amp;custom_custom_sound=HOMER_PUSH_STOP_DOING-04.caf
        </Notification>
    </Message>",

    if
        NotificationEntityId =/= "" ->
            Message2 = Message1 + "<Message operation=\"PUT\" id=\"" ++ NotificationEntityId ++ "\" type=\"LAND/PUSHNOTIFICATION\">
                <Entity>
                    <EntityData>
                        <PushNotification type=\"11\" pushIDLength=\"" ++ integer_to_list(string:len(NotificationPushId)) ++
                        "\" pushID=\"" ++ NotificationPushId ++ "\" hasSource=\"0\" />
                    </EntityData>
                </Entity>
            </Message>";
        NotificationEntityId == "" ->
            Message2 = Message1
    end,

    Message2.

%% DELETE NOTIFICATION
%% DELETE LAND/PUSHNOTIFICATION
land_delete_notification({_, DynVars}) ->
    {ok, ReadNotificationPushId} = ts_dynvars:lookup(notificationPushId, DynVars),
    {ok, NotificationEntityId} = ts_dynvars:lookup(notificationEntityId, DynVars),


    if
        ReadNotificationPushId =/= "" ->
            Return1 = "<Message operation=\"DELETE\" type=\"NOTIFICATION\" id=\"" ++ ReadNotificationPushId ++ "\" />";
        ReadNotificationPushId == "" ->
            Return1 = ""
    end,

    if
        NotificationEntityId =/= "" ->
            Return2 = "<Message operation=\"DELETE\" id=\"" ++ NotificationEntityId ++ "\" type=\"LAND/PUSHNOTIFICATION\" />";
        NotificationEntityId == "" ->
            Return2 = ""
    end,

    Return1 ++ Return2.

%% PUT LAND/BUILDING
land_put_building({Pid, DynVars}) ->
    Sland = load_land_data("land_building", {Pid, DynVars}),

    EntityId = 1000000 + random:uniform(1000000),

    replace(Sland, [
        {"##METHOD##", "PUT"},
        {"##BUILDING_ENTITY_ID##", EntityId},
        {"##BUILDING_ID##", "1044"},
        {"##BUILD_STATE##", "2"},
        {"##CREATED_TIME##", current_time_millis()},
        {"##UPDATED_TIME##", current_time_millis()}
    ]).

%% POST LAND/BUILDING
land_post_building({Pid, DynVars}) ->
    Sland = load_land_data("land_building", {Pid, DynVars}),

    {ok, EntityId} = ts_dynvars:lookup(buildingEntityId, DynVars),
    {ok, Type} = ts_dynvars:lookup(buildingTypeId, DynVars),
    {ok, State} = ts_dynvars:lookup(buildingState, DynVars),
    {ok, CreatedTime} = ts_dynvars:lookup(buildingCreatedTime, DynVars),

    replace(Sland, [
        {"##METHOD##", "POST"},
        {"##BUILDING_ENTITY_ID##", EntityId},
        {"##BUILDING_ID##", Type},
        {"##CREATED_TIME##", CreatedTime},
        {"##UPDATED_TIME##", current_time_millis()},
        {"##BUILD_STATE##", State}
    ]).

%% POST LAND/EVENTCOUNT
land_post_eventcount({_, DynVars}) ->
    {ok, EventCountId} = ts_dynvars:lookup(eventCountId, DynVars),
    {ok, EventCountData} = ts_dynvars:lookup(eventCountData, DynVars),

    if
        EventCountId == "" ->
            "";
        EventCountId =/= "" ->
            "<Message operation=\"POST\" id=\"" ++ EventCountId ++ "\" type=\"LAND/EVENTCOUNT\">
                <Entity>" ++
                    construct_xml(EventCountData) ++
                "</Entity>
            </Message>"
    end.

%% PUT EVENT
land_put_event({_, DynVars}) ->
    {ok, UserId} = ts_dynvars:lookup(userId, DynVars),

    "<Message operation=\"PUT\" type=\"EVENT\">
        <Event toPlayerId=\"" ++ UserId ++ "\" eventType=\"9\">
            <EventData>
                <Request requestTime=\"" ++ integer_to_list(current_time_millis()) ++ "\" requestType=\"9\" type=\"0\" state=\"1\"
                identifier=\"5\" updateTime=\"" ++ integer_to_list(current_time_millis()) ++ "\" />
            </EventData>
        </Event>
    </Message>".

%% Generate the POST Land post body
generate_post_land({Pid, DynVars}) ->
    Vars = {Pid, DynVars},

    Random = random:uniform(1000),

    if
        Random < 200 -> Land1 = land_land(Vars) ++ land_innerland(Vars);
        true -> Land1 = ""
    end,

    if
        Random < 100 -> Land2 = land_put_notice(Vars) ++ land_delete_notice(Vars);
        true -> Land2 = ""
    end,

    if
        Random < 50 -> Land3 = land_character(Vars) ++ land_post_notice(Vars) ++ land_post_job(Vars) ++ land_user(Vars);
        true -> Land3 = ""
    end,

    if
        Random < 40 -> Land4 = land_post_eventcount(Vars) ++ land_sidebar(Vars);
        true -> Land4 = ""
    end,

    if
        Random < 20 -> Land5 = land_post_building(Vars) ++ land_currency(Vars) ++ land_post_quest(Vars);
        true -> Land5 = ""
    end,

    if
        Random < 15 -> Land6 = land_put_quest(Vars) ++ land_put_notification(Vars) ++ land_delete_notification(Vars) ++ land_put_job(Vars) ++ land_delete_job(Vars);
        true -> Land6 = ""
    end,

    if
        Random < 5 -> Land7 = land_put_building(Vars);
        true -> Land7 = ""
    end,

    Land1 ++ Land2 ++ Land3 ++ Land4 ++ Land5 ++ Land6 ++ Land7.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% V2 client
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Generate the limited POST Land call made by the V2 client
generate_v2_post_land({Pid, DynVars}) ->
    Vars = {Pid, DynVars},

    Random = random:uniform(10),

    if
        Random =< 1 -> Land1 = land_land(Vars) ++ land_currency(Vars);
        true -> Land1 = ""
    end,

    if
        Random =< 8 -> Land2 = land_put_event(Vars);
        true -> Land2 = ""
    end,

    Land1 ++ Land2.


%% Generate the Whole Land Update made by the V2 client
%% This function uses the string representation of the XML
generate_v2_whole_land_update({Pid, DynVars}) ->
    {ok, UserId} = ts_dynvars:lookup(userId, DynVars),
    {ok, LandCreatedDate} = ts_dynvars:lookup(landCreatedDate, DynVars),
    {ok, LandData} = ts_dynvars:lookup(landData, DynVars),
    {ok, NextCurrencyId} = ts_dynvars:lookup(nextCurrencyId, DynVars),
    {ok, MiscLandData, NewNextCurrencyId} = generate_v2_whole_land_update_misc({Pid, DynVars}),

    "<Land id=\"" ++ binary:bin_to_list(UserId) ++ "\" updatedAt=\"" ++ integer_to_list(current_time_millis()) ++ "\" createdAt=\"" ++ LandCreatedDate ++ "\"\>" ++
        replace(binary:bin_to_list(LandData), [{"nextCurrencyID=\"" ++ NextCurrencyId ++ "\"", "nextCurrencyID=\"" ++ NewNextCurrencyId ++ "\""}]) ++
    "</Land>\n" ++
    MiscLandData.


%% Generate the Whole Land Update made by the V2 client
%% This function takes the Erlang data structure representation of the XML and recursively constructs it.
%% The XML is huge so this is slow and CPU intensive
generate_v2_whole_land_update_old({Pid, DynVars}) ->
    %% always needed
    {ok, UserId} = ts_dynvars:lookup(userId, DynVars),
    {ok, LandCreatedDate} = ts_dynvars:lookup(landCreatedDate, DynVars),

    case ts_dynvars:lookup(constructedWholeLand, DynVars) of
        {ok, ConstructedWholeLand} ->
            ReturnedLand = ConstructedWholeLand;
        false ->
            {ok, LandData} = ts_dynvars:lookup(landData, DynVars),
            {ok, NextCurrencyId} = ts_dynvars:lookup(nextCurrencyId, DynVars),
            {ok, MiscLandData, NewNextCurrencyId} = generate_v2_whole_land_update_misc({Pid, DynVars}),

            %% Only one Land entry, we're constructing that tag, so just extract the children and serialize them
            [{_, _, TagChildren}|_] = LandData,

            if
                NextCurrencyId == NewNextCurrencyId ->
                    WholeLand = construct_xml(TagChildren);
                true ->
                    WholeLand = replace(construct_xml(TagChildren), [{"nextCurrencyID=\"" ++ NextCurrencyId ++ "\"", "nextCurrencyID=\"" ++ NewNextCurrencyId ++ "\""}])
            end,

            ReturnedLand =  WholeLand ++ "</Land>" ++ MiscLandData,
            ts_dynvars:set(constructedWholeLand, ReturnedLand, DynVars)
    end,

    "<Land id=\"" ++ binary:bin_to_list(UserId) ++ "\" updatedAt=\"" ++ integer_to_list(current_time_millis()) ++ "\" createdAt=\"" ++ LandCreatedDate ++ "\">" ++
        ReturnedLand.



%% Generate the miscellaneous whole land update tags
generate_v2_whole_land_update_misc({_, DynVars}) ->
    {ok, UserId} = ts_dynvars:lookup(userId, DynVars),
    {ok, LNextCurrencyId} = ts_dynvars:lookup(nextCurrencyId, DynVars),

    if
        LNextCurrencyId == "" ->
            NextCurrencyId=1;
        true ->
            NextCurrencyId = list_to_integer(LNextCurrencyId)
    end,

    Random50 = random:uniform(2),

    if
        Random50 == 1 ->
            Random10 = random:uniform(10),
            if
                Random10 == 1 -> %% 3 currency deltas
                    InnerDeltas = "<CurrencyDelta id=\"" ++ LNextCurrencyId ++ "\" reason=\"\" amount=\"+1\" />
                    <CurrencyDelta id=\"" ++ integer_to_list(NextCurrencyId + 1) ++ "\" reason=\"\" amount=\"-1\" />
                    <CurrencyDelta id=\"" ++ integer_to_list(NextCurrencyId + 2) ++ "\" reason=\"\" amount=\"+1\" />",
                    CurrencyIdIncrement = 3;
                Random10 =< 2 -> %% 2 currency deltas
                    InnerDeltas = "<CurrencyDelta id=\"" ++ LNextCurrencyId ++ "\" reason=\"\" amount=\"+1\" />
                    <CurrencyDelta id=\"" ++ integer_to_list(NextCurrencyId + 1) ++ "\" reason=\"\" amount=\"-1\" />",
                    CurrencyIdIncrement = 2;
                true -> %% 1 currency delta
                    InnerDeltas = "<CurrencyDelta id=\"" ++ LNextCurrencyId ++ "\" reason=\"\" amount=\"+1\" />",
                    CurrencyIdIncrement = 1
            end,

            CurrencyDeltas = "<CurrencyDeltas>" ++
                        InnerDeltas ++ "
                    </CurrencyDeltas>\n";
        true ->
            CurrencyDeltas = "<CurrencyDeltas />\n",
            CurrencyIdIncrement = 0
    end,

    EventsProcessed = "<EventsProcessed />\n",

    NotificationId1 = integer_to_list(1000000000000000000000000000000000000000 + random:uniform(9000000000000000000000000000000000000000)),
    NotificationId2 = integer_to_list(1000000000000000000000000000000000000000 + random:uniform(9000000000000000000000000000000000000000)),

    CurrentPushNotifications =
    "<CurrentPushNotifications>
        <Notification id=\"" ++ NotificationId1 ++ "\" toPlayerId=\"" ++ binary:bin_to_list(UserId) ++ "\"
            scheduledIn=\"259200\" templateName=\"thesimpsonstappedout_push_ComeBack3Day\">
            &amp;custom_custom_sound=HOMER_PUSH_STOP_DOING-04.caf&amp;custom_custom_locale=en</Notification>
        <Notification id=\"" ++ NotificationId2 ++ "\" toPlayerId=\"" ++ binary:bin_to_list(UserId) ++ "\"
            scheduledIn=\"604800\" templateName=\"thesimpsonstappedout_push_ComeBack1Week\">
            &amp;custom_custom_sound=HOMER_PUSH_CHEATING-07.caf&amp;custom_custom_locale=en</Notification>
    </CurrentPushNotifications>\n",

    ts_dynvars:set(nextCurrencyId, integer_to_list(NextCurrencyId + CurrencyIdIncrement), DynVars),
    {ok, CurrencyDeltas ++ EventsProcessed ++ CurrentPushNotifications, integer_to_list(NextCurrencyId + CurrencyIdIncrement)}.

