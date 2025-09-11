import { Entity } from '@/types';
import { Loading } from '@/components/loading';

type DataTableColumn<Entry> = {
  title: string;
  field: keyof Entry;
  render?: ({ entry }: { entry: Entry }) => JSX.Element;
};

export type DataTableProps<Entry> = {
  isLoading: boolean;
  data?: Entry[];
  columns: DataTableColumn<Entry>[];
};

export const DataTable = <Entry extends Entity>({
  data,
  columns,
  isLoading,
}: DataTableProps<Entry>) => {
  if (isLoading) {
    return <Loading />;
  }

  if (data?.length === 0) {
    return (
      <div className="h-56 p-4 bg-gray-100 rounded-md flex items-center justify-center">
        <span className="text-gray-600">No Data</span>
      </div>
    );
  }

  return (
    <div className="h-full rounded-md border border-gray-200 bg-white bg-opacity-60">
      <div className="overflow-x-auto">
        <table className="w-full table-auto">
          <thead>
            <tr className="bg-gray-50">
              {columns.map((column, index) => (
                <th 
                  key={column.title + index}
                  className="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider"
                >
                  {column.title}
                </th>
              ))}
            </tr>
          </thead>
          <tbody className="bg-white divide-y divide-gray-200">
            {data?.map((entry, entryIndex) => (
              <tr
                data-testid={`table-row-${entryIndex}`}
                key={entry.id || entryIndex}
                className="hover:bg-gray-50"
              >
                {columns.map(
                  (
                    { field, title, render },
                    columnIndex
                  ) => (
                    <td 
                      key={title + columnIndex}
                      className="px-4 py-4 whitespace-nowrap text-sm text-gray-900"
                    >
                      {render
                        ? render({ entry })
                        : `${entry[field]}`}
                    </td>
                  )
                )}
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </div>
  );
};