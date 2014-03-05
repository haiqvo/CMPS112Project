package unit;

import java.util.Collection;

import troop.Arrower;
import troop.ArrowerCommander;

public class ArrowerSquad extends Squad<Arrower, ArrowerCommander>
{
    public ArrowerSquad(ArrowerCommander c, Collection<Arrower> troops)
    {
        super(c, troops);
    }
}
