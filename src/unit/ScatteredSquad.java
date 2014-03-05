package unit;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import troop.NullSworderCommander;
import troop.Sworder;

public class ScatteredSquad extends SworderSquad
{    
    public ScatteredSquad()
    {
        super(NullSworderCommander.singleton, new ArrayList<Sworder>());
    }
    
    public ScatteredSquad(Collection<Sworder> troops)
    {
        super(NullSworderCommander.singleton, troops);
    }

    public Sworder pop()
    {
        return troops.remove(troops.size() - 1);
    }
    
    @Override
    public Iterator<Sworder> iterator()
    {
        List<Sworder> out = new LinkedList<Sworder>(troops);
        return out.iterator();
    }
}
