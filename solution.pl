% Nurlan Dadashov
% 2019400300
% compiling: yes
% complete: yes

% Solution to BONUS question is also included
% See top_10 predicate.

% knowledge base
:- ['load.pro'].

% helper predicate that finds difference of two lists
difference_of_lists([], [], []).
difference_of_lists([H1|T1], [H2|T2], [H|T]) :- 
    difference_of_lists(T1, T2, T),
    (
        H1 == -1
        -> H is 0;
        H2 == -1
        -> H is 0;
        H is H1 - H2
    ).

% helper predicate that finds squares of all elements of a list
square_of_list([], []).
square_of_list([H1|T1], [H|T]) :- 
    square_of_list(T1, T),
    H is H1*H1.

% helper predicate that finds product of two lists
multiple_of_list([], _, []).
multiple_of_list([H1|T1], [H2|T2], [H|T]) :- 
    multiple_of_list(T1, T2, T),
    H is H1*H2.

% helper predicate that finds sum of all elements of a list
sum_of_list([], 0).
sum_of_list([H|T], TotalSum) :- 
    sum_of_list(T, TempSum),
    TotalSum is H+TempSum.

% 3.1: Given two glanians Name1 and Name2, this predicate will return the distance from Name1 to Name2.
glanian_distance(Name1, Name2, Distance) :- 
    expects(Name1, _, ExpectedFeatures), 
    glanian(Name2, _, GlanianFeatures),
    difference_of_lists(ExpectedFeatures, GlanianFeatures, Difference),
    square_of_list(Difference, Square),
    sum_of_list(Square, Sum),
    Distance is sqrt(Sum).

% 3.2: Given two glanians Name1 and Name2, this predicate will return the weighted distance from Name1 to Name2.
weighted_glanian_distance(Name1, Name2, Distance) :- 
    expects(Name1, _, ExpectedFeatures), 
    glanian(Name2, _, GlanianFeatures),
    weight(Name1, WeightList),
    difference_of_lists(ExpectedFeatures, GlanianFeatures, Difference),
    square_of_list(Difference, Square),
    multiple_of_list(Square, WeightList, Weighted),
    sum_of_list(Weighted, Sum),
    Distance is sqrt(Sum).

% 3.3: This predicate will return a list of cities which contains:  The current city of Name, Name’s LikedCities
find_possible_cities(Name, CityList) :-
    bagof(CityName, 
        (
            city(CityName, List, _), 
            member(Name, List)
        ), 
    NameCity),
    likes(Name, _, LikesCities),
    append(NameCity, LikesCities, CityListTemp),
    list_to_set(CityListTemp, CityList).

% 3.4: Given two glanians Name1 and Name2, this predicate will return the union of the two glanian’s possible cities.
merge_possible_cities(Name1, Name2, MergedCities) :-
    find_possible_cities(Name1, CityList1),
    find_possible_cities(Name2, CityList2),
    append(CityList1, CityList2, MergedCitiesTemp),
    list_to_set(MergedCitiesTemp, MergedCities).

% 3.5 Given two glanians Name1 and Name2, this predicate will return the list of mutual activities of two glanians.
find_mutual_activities(Name1, Name2, MutualActivities) :-
    likes(Name1, LikedActivities1, _),
    likes(Name2, LikedActivities2, _),
    list_to_set(LikedActivities1, LikedActivitiesSet1),
    list_to_set(LikedActivities2, LikedActivitiesSet2),
    intersection(LikedActivitiesSet1, LikedActivitiesSet2, MutualActivities).


% 3.6 find_possible_targets

% finds glanian distances from Name to each Target
apply_glanian_distance_to_list( _ , [], []).
apply_glanian_distance_to_list(Name, [H1|T1], [H|T]) :-
    apply_glanian_distance_to_list(Name, T1, T),
    glanian_distance(Name, H1, D),
    H = D-H1.

% used for filtering lists (used with exclude predicate)
are_identical(X, Y) :-
    X == Y.

% 3.6 This predicate will return a list of possible glanians sorted by their distances as possible matching targets for Name
find_possible_targets(Name, Distances, TargetList) :-
    expects(Name, ExpectedGenders, _),
    findall(GlanianName, 
        (
            glanian(GlanianName, GlanianGender, _),
            member(GlanianGender, ExpectedGenders)
        )
    , Bag2),
    exclude(are_identical(Name), Bag2, Bag),
    apply_glanian_distance_to_list(Name, Bag, Distance_Name_Pair),
    keysort(Distance_Name_Pair, Distance_Name_Pair_Sorted),
    pairs_keys(Distance_Name_Pair_Sorted, Distances),
    pairs_values(Distance_Name_Pair_Sorted, TargetList), !.
    

% 3.7 find_weighted_targets

% finds weighted glanian distances from Name to each Target
apply_weighted_glanian_distance_to_list( _ , [], []).
apply_weighted_glanian_distance_to_list(Name, [H1|T1], [H|T]) :-
    apply_weighted_glanian_distance_to_list(Name, T1, T),
    weighted_glanian_distance(Name, H1, D),
    H = D-H1.

% 3.7 This predicate will return a list of possible glanians sorted by their weighted distances as possible matching targets for Name.
find_weighted_targets(Name, Distances, TargetList) :-
    expects(Name, ExpectedGenders, _),
    findall(GlanianName, 
        (
            glanian(GlanianName, GlanianGender, _), 
            member(GlanianGender, ExpectedGenders)
    ), Bag2),
    exclude(are_identical(Name), Bag2, Bag),
    apply_weighted_glanian_distance_to_list(Name, Bag, Distance_Name_Pair),
    keysort(Distance_Name_Pair, Pair),
    pairs_keys(Pair, Distances),
    pairs_values(Pair, TargetList), !.


% 3.8 find_my_best_target

% checks if features are within Limits
in_range(TargetFeatures, Limits) :-
    length(Limits, Length),
    Length =:= 0, !;
    (Limits = [Lower|[Upper]],
    TargetFeatures > Lower,
    TargetFeatures < Upper).

% checks if a glanian can do an activity in a city
is_compatible(Name, Activity, City) :-
    find_possible_cities(Name, Cities_Possible),
    city(City, _, ActivityList),
    likes(Name, LikedActivities, _),
    dislikes(Name, DislikedActivities, DislikedCities, _),
    ((
        member(City, Cities_Possible),
        \+member(City, DislikedCities),
        member(Activity, ActivityList),
        \+member(Activity, DislikedActivities), 
        !
    );
    (
        \+member(City, DislikedCities),
        member(Activity, ActivityList),
        member(Activity, LikedActivities)
    )).

% analyzes each activity (the list was previously filtered by filter_targets)
% checks compitability, then computes weighted glanian distance 
% and stores as WeightedDistance-Activity-City-Target
% This makes it easier to sort
analyze_activities(_, _, _, [], []).
analyze_activities(Name, Target, City, [HA|TA], OutputList):-
    analyze_activities(Name, Target, City, TA, OutputList1),
    (
        ( 
            is_compatible(Name, HA, City), 
            weighted_glanian_distance(Name, Target, WeightedDistance), 
            OutputList2 = [WeightedDistance-HA-City-Target],
            union(OutputList2, OutputList1, OutputList), !
        );
        OutputList = OutputList1
    ).

% helper predicate for analyze_city
pass_to_analyze_activity(Name, Target, HC, OutputList1, OutputList):-
    city(HC, _, Activities),
    analyze_activities(Name, Target, HC, Activities, OutputList2),
    union(OutputList2, OutputList1, OutputList).

% analyzes each city (the list was previously filtered by filter_targets)
analyze_city(_, _, [], []).
analyze_city(Name, Target, [HC|TC], OutputList) :-
    analyze_city(Name, Target, TC, OutputList1),
    pass_to_analyze_activity(Name, Target, HC, OutputList1, OutputList).

% checks if intersection between activities is less than or equal to 2
check_intersection(Intersection, Length) :-
    length(Intersection, Length),
    Length =< 2.

% analyzes each target (the list was previously filtered by filter_targets)
% checks intersection between activities
analyze_targets(_, [], []).
analyze_targets(Name, [HT|TT], OutputList) :-
    analyze_targets(Name, TT, OutputList1),
    merge_possible_cities(Name, HT, Merged),
    dislikes(Name, DislikedActivities, _, _),
    likes(HT, LikedActivities, _),
    intersection(DislikedActivities, LikedActivities, Intersection),
    (
        (
            check_intersection(Intersection, _), 
            analyze_city(Name, HT, Merged, OutputList2),
            union(OutputList2, OutputList1, OutputList)
        );
        OutputList = OutputList1
    ).

% filters targets by criteria specified (old relations, tolerance limits)
filter_targets(Name, Targets) :-
    find_weighted_targets(Name, _, AllPossibleTargets),
    dislikes(Name, _, _, Limits),
    findall(TargetName, 
        (
            old_relation([Name, TargetName]); 
            old_relation([TargetName, Name])
        ), 
    AllOldRelations),
    subtract(AllPossibleTargets, AllOldRelations, Targets1),
    findall(TargetName, 
        (
            member(TargetName, Targets1),
            glanian(TargetName, _, TargetFeatures),
            maplist(in_range, TargetFeatures, Limits)
        )
    , Targets).

% 3.8 This predicate will use all the other restrictions to find possible matching targets together with possible activities in possible cities.
find_my_best_target(Name, Distances, Activities, Cities, Targets) :-
    filter_targets(Name, Targets2),
    analyze_targets(Name, Targets2, OutputList),
    keysort(OutputList, OutputList2),   
    pairs_keys(OutputList2, OutputList22),
    pairs_values(OutputList2, Targets),
    pairs_keys(OutputList22, OutputList23),
    pairs_values(OutputList22, Cities),
    pairs_keys(OutputList23, Distances),
    pairs_values(OutputList23, Activities), !.

% 3.9 find_my_best_match

% calculates (wgd(Name, Target, Distance) + wgd(Target, Name, Distance)) / 2
wgd(Name, Target, Distance) :-
    weighted_glanian_distance(Name, Target, Distance1),
    weighted_glanian_distance(Target, Name, Distance2),
    Distance is (Distance1 + Distance2) / 2.

% analyzes each activity (the list was previously filtered by filter_targets)
% checks compitability, then computes weighted glanian distance 
% and stores as WeightedDistance-Activity-City-Target
% This makes it easier to sort
analyze_activities_for_best_matches(_, _, _, [], []).
analyze_activities_for_best_matches(Name, Target, City, [HA|TA], OutputList):-
    analyze_activities_for_best_matches(Name, Target, City, TA, OutputList1),
    (
        ( 
            is_compatible(Name, HA, City), 
            is_compatible(Target, HA, City), 
            wgd(Name, Target, WeightedDistance), 
            OutputList2 = [WeightedDistance-HA-City-Target],
            union(OutputList2, OutputList1, OutputList), !
        );
        OutputList = OutputList1
    ).

% helper predicate for analyze_city_for_best_matches
pass_to_analyze_activity_for_best_matches(Name, Target, HC, OutputList1, OutputList):-
    city(HC, _, Activities),
    analyze_activities_for_best_matches(Name, Target, HC, Activities, OutputList2),
    union(OutputList2, OutputList1, OutputList).

% analyzes each city (the list was previously filtered by filter_targets)
analyze_city_for_best_matches(_, _, [], []).
analyze_city_for_best_matches(Name, Target, [HC|TC], OutputList) :-
    analyze_city_for_best_matches(Name, Target, TC, OutputList1),
    pass_to_analyze_activity_for_best_matches(Name, Target, HC, OutputList1, OutputList).

% analyzes each target (the list was previously filtered by filter_targets)
% checks intersection between activities
analyze_targets_for_best_matches(_, [], []).
analyze_targets_for_best_matches(Name, [HT|TT], OutputList) :-
    analyze_targets_for_best_matches(Name, TT, OutputList1),
    ((glanian(Name, Gender, Features),
    expects(HT, TargetExpectedGenders, _),
    member(Gender, TargetExpectedGenders),
    dislikes(HT, TargetDislikedActivities, _, TargetLimits),
    likes(Name, LikedActivities, _),
    maplist(in_range, Features, TargetLimits),
    dislikes(Name, DislikedActivities, _, _),
    likes(HT, TargetLikedActivities, _),
    intersection(DislikedActivities, TargetLikedActivities, Intersection),
    check_intersection(Intersection, _), 
    intersection(TargetDislikedActivities, LikedActivities, Intersection2),
    check_intersection(Intersection2, _),
    merge_possible_cities(Name, HT, Merged),
    analyze_city_for_best_matches(Name, HT, Merged, OutputList2),
    union(OutputList2, OutputList1, OutputList));
    OutputList = OutputList1).

% 3.9 This predicate is similar to the previous predicate with some additional constraints.
find_my_best_match(Name, Distances, Activities, Cities, Targets) :-
    filter_targets(Name, Targets2),
    analyze_targets_for_best_matches(Name, Targets2, OutputList),
    keysort(OutputList, OutputList2),   
    pairs_keys(OutputList2, OutputList22),
    pairs_values(OutputList2, Targets),
    pairs_keys(OutputList22, OutputList23),
    pairs_values(OutputList22, Cities),
    pairs_keys(OutputList23, Distances),
    pairs_values(OutputList23, Activities), !.

% BONUS

% To find best match I used find_weighted_targets predicate
condition(Distance, Name, Target):-
    (
        glanian(Name, _,_),
        find_weighted_targets(Name, Distances, Targets),
        [Distance | _] = Distances,
        [Target | _] = Targets
    ).

% BONUS Top 10 matches will be written to top10.txt, also returned as Top10
% I chack only for weighted distance between glanians and genders -> predicate find_weighted_targets
% top_10(-Top10)
top_10(Top10) :-
    findall(Distance - Name - Target, 
        condition(Distance, Name, Target), 
    Bag), % finds all results from find_weighted_targets predicate
    keysort(Bag, Bag_Sorted),
    (
        length(Top20, 20), 
        append(Top20, _, Bag_Sorted)
    ), % find top 20
    findall(Name-Target, 
        (
            member(_ - Name - Target, Top20)
        ),
    Bag5), % removes distances
    findall(Name - Target,
        (
            member(Name - Target, Bag5),
            member(Target - Name, Bag5)
        ),
    Bag6), % finds permutations
    subtract(Bag5, Bag6, Bag7), % removes permutations
    (
        length(Top10, 10), 
        append(Top10, _, Bag7)
    ), % find top 10
    open('top10.txt', write, OutPutStream), % opens file top10.txt
    printline(Top10, OutPutStream), % writes output
    close(OutPutStream). % closes file top10.txt

% writes result to top10.txt
printline([],_).
printline([H|T], OutPutStream) :-
    writeln(OutPutStream, H),
    printline(T,OutPutStream).
