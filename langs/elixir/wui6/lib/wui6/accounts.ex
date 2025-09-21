defmodule Wui6.Accounts do
  @moduledoc """
  Accounts context with helpers to list users for the admin UI.
  """

  import Ecto.Query, warn: false

  alias Ecto.Changeset
  alias Ecto.QueryError
  alias Wui6.Accounts.{Session, SessionRequest, User, UserRole}
  alias Wui6.Accounts.SessionNotifier
  alias Wui6.Roles
  alias Wui6.Repo

  @type user :: %User{
          id: integer() | nil,
          email: String.t() | nil,
          inserted_at: NaiveDateTime.t() | DateTime.t() | nil,
          last_activity: NaiveDateTime.t() | DateTime.t() | nil,
          enabled: boolean() | nil,
          roles: list(Wui6.Roles.Role.t())
        }

  @doc """
  List up to `limit` users ordered by identifier, supporting optional filtering and pagination.
  """
  @spec list_users(keyword()) :: %{
          entries: list(user()),
          has_more?: boolean(),
          next_page: integer() | nil,
          prev_page: integer() | nil,
          page: integer()
        }
  def list_users(opts \\ []) do
    limit = Keyword.get(opts, :limit, 10)
    page = opts |> Keyword.get(:page, 1) |> max(1)
    query = sanitize_query(Keyword.get(opts, :query))
    role_filter = Keyword.get(opts, :role_id)

    case do_list_users(limit, page, query, role_filter) do
      {:ok, page_map} -> page_map
      {:error, _reason} -> fallback_page(limit, page, query, role_filter)
    end
  end

  defp do_list_users(limit, page, query, role_filter) do
    case fetch_page(limit, page, query, role_filter) do
      {:ok, %{entries: []}} when page > 1 ->
        do_list_users(limit, page - 1, query, role_filter)

      {:ok, page_map} ->
        {:ok, page_map}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp fetch_page(limit, page, query, role_filter) do
    offset = max(page - 1, 0) * limit

    queryable =
      User
      |> order_by([u], asc: u.id)
      |> maybe_filter_email(query)
      |> maybe_filter_role(role_filter)
      |> distinct(true)
      |> offset(^offset)
      |> limit(^(limit + 1))

    entries =
      queryable
      |> Repo.all()
      |> Repo.preload(:roles)

    {:ok, build_page(entries, limit, page)}
  rescue
    error in [QueryError, DBConnection.ConnectionError] -> {:error, error}
    error -> {:error, error}
  end

  defp build_page(entries_with_extra, limit, page) do
    has_more? = length(entries_with_extra) > limit
    entries = Enum.take(entries_with_extra, limit)

    %{
      entries: entries,
      has_more?: has_more?,
      page: page,
      prev_page: if(page > 1, do: page - 1, else: nil),
      next_page: if(has_more?, do: page + 1, else: nil)
    }
  end

  defp maybe_filter_email(queryable, ""), do: queryable

  defp maybe_filter_email(queryable, query) do
    where(queryable, [u], ilike(u.email, ^("%" <> query <> "%")))
  end

  defp maybe_filter_role(queryable, nil), do: queryable

  defp maybe_filter_role(queryable, role_filter) do
    case normalize_id(role_filter) do
      nil ->
        queryable

      role_id ->
        queryable
        |> join(:inner, [u], r in assoc(u, :roles))
        |> where([_u, r], r.id == ^role_id)
    end
  end

  defp fallback_page(_limit, page, _query, _role_filter) do
    %{
      entries: [],
      has_more?: false,
      page: page,
      prev_page: if(page > 1, do: page - 1, else: nil),
      next_page: nil
    }
  end

  @doc """
  Returns up to `limit` users ordered by oldest activity first.

  Users without recorded activity are listed before those with a timestamp. In
  case of database errors the function simply returns an empty list so the admin
  view can degrade gracefully.
  """
  @spec list_stale_users(keyword()) :: list(user())
  def list_stale_users(opts \\ []) do
    limit = opts |> Keyword.get(:limit, 10) |> normalize_limit()

    query =
      from(u in User,
        order_by: [asc_nulls_first: u.last_activity, asc: u.inserted_at, asc: u.id],
        limit: ^limit
      )

    try do
      query
      |> Repo.all()
      |> Repo.preload(:roles)
    rescue
      _error -> []
    end
  end

  @doc """
  List sessions ordered by the oldest access time, optionally filtered by user email.

  Returns an empty list if the query fails so the admin view can degrade gracefully.
  """
  @spec list_sessions(keyword()) :: list(Session.t())
  def list_sessions(opts \\ []) do
    query = sanitize_query(Keyword.get(opts, :query))

    Session
    |> join(:inner, [s], u in assoc(s, :user))
    |> maybe_filter_sessions_by_email(query)
    |> order_by([s, _u], asc: s.last_access, asc: s.created_at, asc: s.id)
    |> preload([_s, u], user: u)
    |> Repo.all()
  rescue
    _ -> []
  end

  @doc """
  Generates a session request for the given email and delivers the registration link.

  Returns `{:ok, request}` containing the generated token or an error tuple when the
  email cannot be associated with an existing user.
  """
  @spec send_session_registration_link(String.t(), keyword()) ::
          {:ok, SessionRequest.t()} | {:error, :not_found | term()}
  def send_session_registration_link(email, opts \\ [])

  def send_session_registration_link(email, opts) when is_binary(email) do
    normalized = normalize_email(email)

    if normalized == "" do
      {:error, :invalid_email}
    else
      with {:ok, %User{} = user} <- fetch_or_create_user_by_email(normalized),
           {:ok, %SessionRequest{} = request} <- generate_session_request(user),
           :ok <- deliver_session_request(request, opts) do
        {:ok, request}
      else
        {:error, :not_found} -> {:error, :not_found}
        {:error, reason} -> {:error, reason}
      end
    end
  end

  def send_session_registration_link(_, _), do: {:error, :invalid_email}

  @doc """
  Consumes a pending session request token, creates the persistent session and returns it.
  """
  @spec finalize_session_registration(String.t(), %{data: map(), timestamp: NaiveDateTime.t()}) ::
          {:ok, Session.t()} | {:error, term()}
  def finalize_session_registration(token, %{data: context, timestamp: timestamp} = snapshot)
      when is_binary(token) and is_map(context) do
    Repo.transaction(fn ->
      case Repo.get_by(SessionRequest, token: token) |> Repo.preload(:user) do
        nil ->
          Repo.rollback(:not_found)

        request ->
          Repo.delete!(request)

          context_payload = encode_initial_context(snapshot)

          %Session{}
          |> Session.changeset(%{
            user_id: request.user_id,
            token: token,
            last_access: timestamp,
            context_info: context_payload,
            created_at: timestamp
          })
          |> Repo.insert()
          |> case do
            {:ok, session} -> Repo.preload(session, user: :roles)
            {:error, changeset} -> Repo.rollback({:error, changeset})
          end
      end
    end)
    |> case do
      {:ok, session} -> {:ok, session}
      {:error, reason} -> {:error, reason}
    end
  end

  def finalize_session_registration(_, _), do: {:error, :invalid_token}

  @doc """
  Retrieves a session (with user preloaded) for the provided token, if any.
  """
  @spec get_session_with_user_by_token(String.t() | nil) :: Session.t() | nil
  def get_session_with_user_by_token(token) when is_binary(token) do
    case Repo.get_by(Session, token: token) do
      nil -> nil
      session -> Repo.preload(session, user: :roles)
    end
  end

  def get_session_with_user_by_token(_), do: nil

  @doc """
  Updates the stored session context and last access snapshot, helping detect anomalies.
  """
  @spec refresh_session_snapshot(String.t(), %{data: map(), timestamp: NaiveDateTime.t()}) ::
          {:ok, Session.t()} | {:error, term()}
  def refresh_session_snapshot(token, %{data: context, timestamp: timestamp} = snapshot)
      when is_binary(token) and is_map(context) do
    Repo.transaction(fn ->
      case Repo.get_by(Session, token: token) do
        nil ->
          nil

        session ->
          payload = merge_context_info(session.context_info, snapshot)

          session
          |> Session.changeset(%{last_access: timestamp, context_info: payload})
          |> Repo.update()
          |> case do
            {:ok, updated} -> Repo.preload(updated, user: :roles)
            {:error, changeset} -> Repo.rollback({:error, changeset})
          end
      end
    end)
    |> case do
      {:ok, %Session{} = session} -> {:ok, session}
      {:ok, nil} -> {:error, :not_found}
      {:error, reason} -> {:error, reason}
    end
  end

  def refresh_session_snapshot(_, _), do: {:error, :invalid_token}

  @doc """
  Deletes the persisted session identified by the given token.
  """
  @spec delete_session_by_token(String.t() | nil) :: :ok | {:error, term()}
  def delete_session_by_token(token) when is_binary(token) do
    case Repo.get_by(Session, token: token) do
      %Session{} = session ->
        case Repo.delete(session) do
          {:ok, _} -> :ok
          {:error, reason} -> {:error, reason}
        end

      _ ->
        {:error, :not_found}
    end
  end

  def delete_session_by_token(_), do: {:error, :invalid_token}

  @doc """
  Lists all active sessions for the given user ordered by most recent access.
  """
  @spec list_sessions_for_user(integer()) :: list(Session.t())
  def list_sessions_for_user(user_id) when is_integer(user_id) do
    Session
    |> where([s], s.user_id == ^user_id)
    |> order_by([s], desc: s.last_access, desc: s.created_at, desc: s.id)
    |> preload(:user)
    |> Repo.all()
  rescue
    _ -> []
  end

  def list_sessions_for_user(_), do: []

  @doc """
  Fetch a user with preloaded roles. Falls back to user with ID 1 if the requested user is missing.
  """
  @spec get_user_with_roles(term()) :: {:ok, user()} | {:error, :not_found}
  def get_user_with_roles(id) do
    id
    |> normalize_id()
    |> fetch_user_with_roles()
    |> case do
      {:ok, %User{} = user} -> {:ok, user}
      _ -> fetch_user_with_roles(1)
    end
    |> case do
      {:ok, %User{} = user} ->
        {:ok, user}

      _ ->
        case Repo.one(from u in User, order_by: [asc: u.id], limit: 1) do
          nil -> {:error, :not_found}
          %User{} = user -> {:ok, Repo.preload(user, :roles)}
        end
    end
  end

  defp normalize_id(id) when is_integer(id), do: id

  defp normalize_id(id) when is_binary(id) do
    case Integer.parse(id) do
      {int, ""} when int > 0 -> int
      _ -> nil
    end
  end

  defp normalize_id(_), do: nil

  defp fetch_user_with_roles(nil), do: {:error, :invalid_id}

  defp fetch_user_with_roles(id) do
    case Repo.get(User, id) do
      %User{} = user -> {:ok, Repo.preload(user, :roles)}
      _ -> {:error, :not_found}
    end
  end

  @doc """
  List roles available for assignment to the given user (i.e. not already assigned).
  """
  @spec available_roles_for_user(user()) :: list(Wui6.Roles.Role.t())
  def available_roles_for_user(%User{roles: roles}) do
    roles = roles || []
    taken_ids = MapSet.new(Enum.map(roles, & &1.id))

    Roles.list_roles()
    |> Enum.reject(fn role -> MapSet.member?(taken_ids, role.id) end)
  end

  @doc """
  Search users by email and annotate whether they already belong to the given role.

  Returns up to `limit` results (default 3). When the query is blank or invalid, an empty list is returned.
  """
  @spec search_users_for_role(integer(), keyword()) ::
          list(%{id: integer(), email: String.t(), enabled: boolean() | nil, has_role: boolean()})
  def search_users_for_role(role_id, opts \\ [])

  def search_users_for_role(role_id, opts) when is_integer(role_id) and role_id > 0 do
    limit = opts |> Keyword.get(:limit, 3) |> normalize_limit()
    query = sanitize_query(Keyword.get(opts, :query))

    if query == "" do
      []
    else
      page = list_users(limit: limit, query: query)
      assigned_ids = role_membership_ids(role_id)

      page.entries
      |> Enum.map(fn %User{} = user ->
        %{
          id: user.id,
          email: user.email,
          enabled: Map.get(user, :enabled),
          has_role: MapSet.member?(assigned_ids, user.id)
        }
      end)
    end
  rescue
    _ -> []
  end

  def search_users_for_role(_, _), do: []

  @doc """
  Assign a role to a user. Returns `:ok` even if the role was already assigned.
  """
  @spec assign_role_to_user(integer(), integer()) :: :ok | {:error, Ecto.Changeset.t()}
  def assign_role_to_user(user_id, role_id) when is_integer(user_id) and is_integer(role_id) do
    %UserRole{}
    |> UserRole.changeset(%{user_id: user_id, role_id: role_id})
    |> Repo.insert(on_conflict: :nothing, conflict_target: [:user_id, :role_id], returning: false)
    |> case do
      {:ok, _} -> :ok
      {:error, %Ecto.Changeset{} = changeset} -> {:error, changeset}
    end
  end

  def assign_role_to_user(_, _), do: {:error, :invalid_ids}

  @doc """
  Remove a role assignment from a user. Returns `:ok` even if no association existed.
  """
  @spec remove_role_from_user(integer(), integer()) :: :ok | {:error, term()}
  def remove_role_from_user(user_id, role_id) when is_integer(user_id) and is_integer(role_id) do
    from(ur in UserRole, where: ur.user_id == ^user_id and ur.role_id == ^role_id)
    |> Repo.delete_all()
    |> case do
      {_count, _} -> :ok
    end
  end

  def remove_role_from_user(_, _), do: {:error, :invalid_ids}

  defp maybe_filter_sessions_by_email(queryable, ""), do: queryable

  defp maybe_filter_sessions_by_email(queryable, query) do
    where(queryable, [_s, u], ilike(u.email, ^("%" <> query <> "%")))
  end

  defp fetch_or_create_user_by_email(email) when is_binary(email) do
    normalized = normalize_email(email)

    case Repo.get_by(User, email: normalized) do
      %User{} = user -> {:ok, user}
      nil -> create_user_with_email(normalized)
      _ -> {:error, :not_found}
    end
  end

  defp fetch_or_create_user_by_email(_), do: {:error, :invalid_email}

  defp create_user_with_email(email) do
    %User{}
    |> Changeset.change(email: email)
    |> Repo.insert()
    |> case do
      {:ok, %User{} = user} ->
        {:ok, user}

      {:error, %Changeset{} = changeset} ->
        if email_conflict?(changeset) do
          case Repo.get_by(User, email: email) do
            %User{} = user -> {:ok, user}
            _ -> {:error, changeset}
          end
        else
          {:error, changeset}
        end
    end
  end

  defp email_conflict?(changeset) do
    Enum.any?(changeset.errors, fn
      {:email, {_msg, opts}} -> opts[:constraint] == :unique
      _ -> false
    end)
  end

  defp generate_session_request(%User{} = user) do
    Repo.transaction(fn ->
      from(sr in SessionRequest, where: sr.user_id == ^user.id)
      |> Repo.delete_all()

      attrs = %{
        user_id: user.id,
        email: user.email,
        token: generate_session_token()
      }

      %SessionRequest{}
      |> SessionRequest.changeset(attrs)
      |> Repo.insert()
      |> case do
        {:ok, request} -> request
        {:error, changeset} -> Repo.rollback({:error, changeset})
      end
    end)
    |> case do
      {:ok, request} -> {:ok, request}
      {:error, reason} -> {:error, reason}
    end
  end

  defp deliver_session_request(%SessionRequest{} = request, opts) do
    case Keyword.fetch(opts, :url_builder) do
      {:ok, builder} when is_function(builder, 1) ->
        case builder.(request) do
          url when is_binary(url) and byte_size(url) > 0 ->
            SessionNotifier.deliver_register_link(request, url)

          _ ->
            {:error, :invalid_url}
        end

      {:ok, _other} ->
        {:error, :invalid_url_builder}

      :error ->
        {:error, :missing_url_builder}
    end
  end

  defp deliver_session_request(_, _), do: {:error, :invalid_request}

  defp encode_initial_context(%{data: context}) do
    Jason.encode!(%{"initial" => context, "last_seen" => context, "alerts" => []})
  end

  defp merge_context_info(existing, %{data: snapshot}) do
    with {:ok, map} <- decode_context(existing) do
      initial = Map.get(map, "initial", snapshot)
      alerts = Map.get(map, "alerts", [])

      alerts =
        alerts
        |> maybe_add_alert(initial["ip"], snapshot["ip"], "ip_mismatch", snapshot)
        |> maybe_add_alert(
          initial["user_agent"],
          snapshot["user_agent"],
          "user_agent_mismatch",
          snapshot
        )

      updated = %{
        "initial" => initial,
        "last_seen" => snapshot,
        "alerts" => alerts
      }

      Jason.encode!(updated)
    else
      _ -> default_context_payload(snapshot)
    end
  end

  defp default_context_payload(snapshot) do
    Jason.encode!(%{"initial" => snapshot, "last_seen" => snapshot, "alerts" => []})
  end

  defp decode_context(nil), do: {:error, :invalid}
  defp decode_context(value) when is_binary(value), do: Jason.decode(value)
  defp decode_context(_), do: {:error, :invalid}

  defp maybe_add_alert(alerts, expected, observed, type, snapshot) do
    cond do
      is_nil(expected) or is_nil(observed) ->
        alerts

      expected == observed ->
        alerts

      true ->
        alert = %{
          "type" => type,
          "expected" => expected,
          "observed" => observed,
          "at" => snapshot["at"]
        }

        Enum.concat(alerts, [alert])
    end
  end

  defp normalize_limit(limit) when is_integer(limit) and limit > 0 do
    limit
    |> min(25)
    |> max(1)
  end

  defp normalize_limit(_), do: 3

  defp role_membership_ids(role_id) do
    role_id
    |> list_users_for_role()
    |> Enum.map(& &1.id)
    |> MapSet.new()
  end

  @doc """
  List users who currently have the given role assigned.
  """
  @spec list_users_for_role(integer()) :: list(user())
  def list_users_for_role(role_id) when is_integer(role_id) do
    from(u in User,
      join: ur in UserRole,
      on: ur.user_id == u.id,
      where: ur.role_id == ^role_id,
      order_by: [asc: u.email]
    )
    |> Repo.all()
  rescue
    _ -> []
  end

  def list_users_for_role(_), do: []

  @doc """
  Toggle whether a user is enabled.
  """
  @spec toggle_user_enabled(integer()) :: {:ok, user()} | {:error, term()}
  def toggle_user_enabled(user_id) when is_integer(user_id) do
    case Repo.get(User, user_id) do
      nil ->
        {:error, :not_found}

      %User{} = user ->
        current = Map.get(user, :enabled, true)

        user
        |> Changeset.change(enabled: not current)
        |> Repo.update()
    end
  rescue
    error in [QueryError, DBConnection.ConnectionError] ->
      {:error, error}

    error ->
      {:error, error}
  end

  def toggle_user_enabled(_), do: {:error, :invalid_id}

  defp sanitize_query(nil), do: ""

  defp sanitize_query(query) do
    query
    |> to_string()
    |> String.trim()
  end

  defp normalize_email(email) when is_binary(email) do
    email
    |> String.trim()
    |> String.downcase()
  end

  defp generate_session_token do
    bytes = :crypto.strong_rand_bytes(32)
    hash = :crypto.hash(:sha, bytes)
    Base.encode16(hash, case: :lower)
  end
end
