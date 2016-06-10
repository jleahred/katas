defmodule Session do
@moduledoc """
Here is defined Session structs

* Session.Acceptor
* Session.Initiator
* Session.Receptor
* Session.Sender
* Session.Status

"""

    defmodule  Receptor do
        @moduledoc false
        defstruct   msg_seq_num:       1
    end

    defmodule  Sender  do
        @moduledoc false
        defstruct   msg_seq_num:       1
    end

    defmodule  Status  do
        @moduledoc false
        defstruct   connect_role:     :acceptor,        # :initiator
                    status:           :waitting_login,  # :login_ok
                    me_comp_id:       "ACCEPTOR",
                    other_comp_id:    "INITIATOR",
                    password:         "",
                    fix_version:      "",
                    heartbeat_interv:  0,
                    receptor:         %Receptor{},
                    sender:           %Sender{}
    end

end
