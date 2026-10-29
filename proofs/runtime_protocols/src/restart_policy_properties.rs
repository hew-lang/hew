use super::*;
use proptest::prelude::*;
proptest! {
    #![proptest_config(ProptestConfig{cases:1024,..ProptestConfig::default()})]
    #[test]
    fn generated_restart_groups_match_declarations_and_spent_policy(
        rows in prop::collection::btree_map(any::<u16>(),(any::<bool>(),any::<bool>()),1..20),
        strategy in 0i32..4, failed_index in any::<usize>(), reverse in any::<bool>()
    ){
        let mut roster:Vec<_>=rows.into_iter().enumerate().map(|(index,(identity,(nested,spent)))|{
            (if nested{RestartChildRole::Supervisor{index,identity:u64::from(identity)}}else{RestartChildRole::Actor(u64::from(identity))},spent)
        }).collect();
        if reverse{roster.reverse();}
        let failed=roster[failed_index%roster.len()].0;
        let can_restart=roster.iter().any(|(role,spent)|*role==failed && !spent);
        let mut expected=Vec::new();
        if can_restart{
            for &(role,spent) in &roster{
                let eligible=match strategy{0|3=>role==failed,1=>true,_=>role.identity()>=failed.identity()};
                if eligible && !spent{expected.push(role);}
            }
        }
        expected.sort_by_key(|role|role.identity());
        prop_assert_eq!(choose_roles(roster.into_iter(),strategy,failed),expected);
    }
}
