using System;
using System.Drawing;
using System.Collections.Generic;

using SwinGame;
using Graphics = SwinGame.Graphics;
using Bitmap = SwinGame.Bitmap;
using Font = SwinGame.Font;
using FontStyle = SwinGame.FontStyle;
using Event = SwinGame.Event;
using CollisionSide = SwinGame.CollisionSide;
using Sprite = SwinGame.Sprite;

using GameResources;

namespace TomatoQuest
{
    public class Inventory
    {
        public List<Item> Items = new List<Item>();

        //Adds the item to the Inventory
        public void AddItem(Item item)
        {
            Items.Add(item);
        }

        //Removes the item from the Inventory
        public void RemoveItem(Item item)
        {
            Items.Remove(item);
        }

        public void DropItems()
        {
            //For every item in the character inventory, drop the item
            for (int i = 0; i < Items.Count; i++)
            {
                Items[i].Dropped = true;
                RemoveItem(Items[i]);
            }
        }

        //Gets whether the Inventory has any items
        public bool HasItems()
        {
            if (Items.Count > 0)
            {
                return true;
            }
            else
            {
                return false;
            }
        }

        //Checks if the Inventory has the Item
        public bool HaveItem(Item item)
        {
            //If the Inventory Contains the item, return true, else return false
            if (Items.Contains(item))
            {
                return true;
            }
            else
            {
                return false;
            }
        }
    }
}
