#[macro_use]
extern crate rpds;

mod model;
mod scheduler;

fn main() {
    let init_config: model::InitConfig = serde_yaml::from_str(init_config_yaml()).unwrap();

    println!(
        "\ninitial config:\n{}",
        serde_yaml::to_string(&init_config).unwrap()
    );

    let result_status = scheduler::generate_schedule_from_init_config(&init_config).unwrap();
    let result_yaml = serde_yaml::to_string(&result_status).unwrap();

    println!("--------------------------------------");
    println!("Final status");
    println!("{}", result_yaml);
}

fn init_config_yaml() -> &'static str {
    r#"
---
# -------------- database documents for recipes  ------------------
recipes_db:
  recipe1:
    description: recipe1
    processes:
          proc1:
            description: proc1
            inputs:
              - product11
              - product2
            outputs:
              - prod_id: prod3
                valid_for: 15m
              - prod_id: prod4
                valid_for: 15m
            sequence:
              - do first1
              - do second1
            required_time: 10m
          proc2:
            description: proc2
            inputs:
              - product2
              - product3
            outputs:
              - prod_id: prod5
                valid_for: 15m
              - prod_id: prod6
                valid_for: 15m
            sequence:
              - do first11
              - do second11
            required_time: 10m

# ----------  initial configuration   ---------
recipes_todo:
    - recipe_id: recipe1
      priority: Low
      # pending ends_before
available_products:
    - product: 
        prod_id: product1
        valid_for: 15m
      available_at: 7m
    - product: 
        prod_id: product2
        valid_for: 15m
      available_at: 7m
"#
}
