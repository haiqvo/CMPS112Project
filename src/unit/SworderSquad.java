package unit;

import java.util.Collection;

import troop.Sworder;
import troop.SworderCommander;


public class SworderSquad extends Squad<Sworder, SworderCommander>
{
    public SworderSquad(SworderCommander c, Collection<Sworder> troops)
    {
        super(c, troops);
    }
}
