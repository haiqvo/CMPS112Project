package troop;

public interface TroopBase
{
    public int getUpgrades();
    public int getStrength();
    public void upgradeStrength();
    public void unupgradeStrength();
    public void sufferQueuedDamage();
    public void sufferDamage(int damage);
    public void queueDamage(int damage);
    public String toString();
}
