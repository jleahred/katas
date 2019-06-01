defmodule JLE.AlarmsSch do
  defmodule Cli do
    use Ecto.Schema

    @primary_key {:id_cli_alarms, :integer, autogenerate: false}

    # source: :id_cli_alarms
    schema "CLI_ALARMS" do
      # field(:id_cli_alarms, :integer)
      field(:db_time, :naive_datetime)
      field(:gen_time, :naive_datetime)
      field(:sent_time, :naive_datetime)
      field(:rec_time, :naive_datetime)
      field(:al_id, :integer)
      field(:priority, :integer)
      field(:al_type, :integer)
      field(:broker_code, :string)
      field(:process_name, :string)
      field(:machine, :string)
      field(:subject, :string)
      field(:description, :string)
      field(:process_version, :string)
      field(:process_uuid, :string)
      field(:source, :string)
    end
  end

  defmodule Srv do
    use Ecto.Schema

    @primary_key {:id_srv_alarms, :integer, autogenerate: false}
    schema "SRV_ALARMS" do
      # field(:id_srv_alarms, :integer)
      field(:db_time, :naive_datetime)
      field(:gen_time, :naive_datetime)
      field(:sent_time, :naive_datetime)
      field(:rec_time, :naive_datetime)
      field(:al_id, :integer)
      field(:priority, :integer)
      field(:al_type, :integer)
      field(:broker_code, :string)
      field(:process_name, :string)
      field(:machine, :string)
      field(:subject, :string)
      field(:description, :string)
      field(:process_version, :string)
      field(:process_uuid, :string)
      field(:source, :string)
    end
  end
end
