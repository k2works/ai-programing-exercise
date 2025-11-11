namespace SalesManagement.Api.Dtos;

/// <summary>
/// ページングレスポンスDTO
/// </summary>
public class PageResponse<T>
{
    public List<T> Content { get; set; }
    public int Page { get; set; }
    public int Size { get; set; }
    public long Total { get; set; }
    public int TotalPages { get; set; }

    public PageResponse(List<T> content, int page, int size, long total)
    {
        Content = content;
        Page = page;
        Size = size;
        Total = total;
        TotalPages = (int)Math.Ceiling((double)total / size);
    }
}
