package engagement;

import java.util.Collection;
import java.util.Iterator;

public interface EngagableGroup<T extends Engagable> extends Engagable
{
    public T get(int index);
    public Collection<T> getAll();
    public Iterator<T> iterator();
    public int size();
    public boolean remove(T e);
}
